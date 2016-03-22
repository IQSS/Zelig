#' Rare Events Logistic Regression for Dichotomous Dependent Variables
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-relogit.html}
#' @import methods
#' @export Zelig-relogit
#' @exportClass Zelig-relogit
#'
#' @include model-zelig.R
#' @include model-glm.R
#' @include model-binchoice.R
#' @include model-logit.R

zrelogit <- setRefClass("Zelig-relogit",
                      contains = "Zelig",
                      fields = list(family = "character",
                                    link = "character",
                                    linkinv = "function"))

zrelogit$methods(
  initialize = function() {
    callSuper()
    .self$name <- "relogit"
    .self$description <- "Rare Events Logistic Regression for Dichotomous Dependent Variables"
    .self$fn <- quote(relogit)
    .self$family <- "binomial"
    .self$link <- "logit"
    .self$wrapper <- "relogit"
    ref1<-bibentry(
            bibtype="Article",
            title = "Logistic Regression in Rare Events Data",
            author = c(
                person("Gary", "King"),
                person("Langche", "Zeng")
                ),
            journal = "Political Analysis",
            volume = 9,
            number = 2,
            year = 2001,
            pages = "137--163")
    ref2<-bibentry(
            bibtype="Article",
            title = "Explaining Rare Events in International Relations",
            author = c(
                person("Gary", "King"),
                person("Langche", "Zeng")
                ),
            journal = "International Organization",
            volume = 55,
            number = 3,
            year = 2001,
            pages = "693--715")
    .self$refs<-c(.self$refs,ref1,ref2)
  }
)

zrelogit$methods(
  zelig = function(formula, ..., tau = NULL, bias.correct = NULL, case.control = NULL, data, by = NULL, bootstrap = FALSE) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    # Catch NULL case.control
    if (is.null(case.control))
      case.control <- "prior"
    # Catch NULL bias.correct
    if (is.null(bias.correct))
      bias.correct = TRUE
    # Construct formula. Relogit models have the structure:
    #   cbind(y, 1-y) ~ x1 + x2 + x3 + ... + xN
    # Where y is the response.
    form <- update(formula, cbind(., 1 - .) ~ .)
    .self$model.call$formula <- form
    .self$model.call$case.control <- case.control
    .self$model.call$bias.correct <- bias.correct
    .self$model.call$tau <- tau
    callSuper(formula = formula, data = data, ..., weights = NULL, by = by, bootstrap = bootstrap)
  }
)

zrelogit$methods(
  qi = function(simparam, mm) {
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    coeff <- simparam
    eta <- simparam %*% t(mm)
    eta <- Filter(function (y) !is.na(y), eta)
    theta <- matrix(.self$linkinv(eta), nrow = nrow(coeff))
    ev <- matrix(.self$linkinv(eta), ncol = ncol(theta))
    pv <- matrix(nrow = nrow(ev), ncol = ncol(ev))
    for (j in 1:ncol(ev))
      pv[, j] <- rbinom(length(ev[, j]), 1, prob = ev[, j])
    levels(pv) <- c(0, 1)
    return(list(ev = ev, pv = pv))
  }
)


#' Estimation function for rare events logit models
#' @keywords internal
relogit <- function(formula,
                    data = sys.parent(),
                    tau = NULL,
                    bias.correct = TRUE,
                    case.control = "prior",
                    ...){
  mf <- match.call()
  mf$tau <- mf$bias.correct <- mf$case.control <- NULL
  if (!is.null(tau)) {
    tau <- unique(tau)
    if (length(case.control) > 1)
      stop("You can only choose one option for case control correction.")
    ck1 <- grep("p", case.control)
    ck2 <- grep("w", case.control)
    if (length(ck1) == 0 & length(ck2) == 0)
      stop("choose either case.control = \"prior\" ",
           "or case.control = \"weighting\"")
    if (length(ck2) == 0)
      weighting <- FALSE
    else 
      weighting <- TRUE
  }
  else
    weighting <- FALSE
  if (length(tau) > 2)
    stop("tau must be a vector of length less than or equal to 2")
  else if (length(tau) == 2) {
    mf[[1]] <- relogit
    res <- list()
    mf$tau <- min(tau)
    res$lower.estimate <- eval(as.call(mf), parent.frame())
    mf$tau <- max(tau)
    res$upper.estimate <- eval(as.call(mf), parent.frame())
    res$formula <- formula
    class(res) <- c("Relogit2", "Relogit")
    return(res)
  }
  else {
    mf[[1]] <- glm
    mf$family <- binomial(link = "logit")
    y2 <- model.response(model.frame(mf$formula, data))
    if (is.matrix(y2))
      y <- y2[,1]
    else
      y <- y2
    ybar <- mean(y)
    if (weighting) {
      w1 <- tau / ybar
      w0 <- (1-tau) / (1-ybar)
      wi <- w1 * y + w0 * (1 - y)
      mf$weights <- wi
    }
    res <- eval(as.call(mf), parent.frame())
    res$call <- match.call(expand.dots = TRUE)
    res$tau <- tau
    X <- model.matrix(res)
    ## bias correction
    if (bias.correct){
      pihat <- fitted(res)
      if (is.null(tau)) # w_i = 1
        wi <- rep(1, length(y))
      else if (weighting) 
        res$weighting <- TRUE
      else {
        w1 <- tau/ybar
        w0 <- (1 - tau) / (1 - ybar)
        wi <- w1 * y + w0 * (1 - y)
        res$weighting <- FALSE
      }
      W <- pihat * (1 - pihat) * wi
      ##Qdiag <- diag(X%*%solve(t(X)%*%diag(W)%*%X)%*%t(X))
      Qdiag <- lm.influence(lm(y ~ X - 1, weights = W))$hat / W
      if (is.null(tau)) # w_1=1 since tau=ybar
        xi <- 0.5 * Qdiag * (2 * pihat - 1)
      else
        xi <- 0.5 * Qdiag * ((1 + w0) * pihat - w0)
      res$coefficients <- res$coefficients -
        lm(xi ~ X - 1, weights = W)$coefficients
      res$bias.correct <- TRUE
    }
    else
      res$bias.correct <- FALSE
    ## prior correction 
    if (!is.null(tau) & !weighting){      
      if (tau <= 0 || tau >= 1) 
        stop("\ntau needs to be between 0 and 1.\n") 
      res$coefficients["(Intercept)"] <- res$coefficients["(Intercept)"] - 
        log(((1 - tau) / tau) * (ybar / (1 - ybar)))
      res$prior.correct <- TRUE
      res$weighting <- FALSE
    }
    else
      res$prior.correct <- FALSE
    if (is.null(res$weighting))
      res$weighting <- FALSE
    
    res$linear.predictors <- t(res$coefficients) %*% t(X) 
    res$fitted.values <- 1 / (1 + exp(-res$linear.predictors))
    res$zelig <- "Relogit"
    class(res) <- c("Relogit", "glm")
    return(res)
  }
}
