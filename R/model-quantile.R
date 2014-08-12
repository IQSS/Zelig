library(quantreg)

#' @include model-zelig.R
zquantile <- setRefClass("Zelig-quantile",
                         contains="Zelig",
                         field=list(tau = "ANY"
                         ))

zquantile$methods(
  initialize = function() {
    callSuper()
    .self$fn <- quote(quantreg::rq)
    .self$name <- "quantile"
    .self$authors <- "Alexander D'Amour"
    .self$year <- 2008
    .self$category <- "continuous"
    .self$description <- "Quantile Regression for Continuous Dependent Variables"
    # JSON
    .self$outcome <- "continuous"
  }
)

zquantile$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)
    if (!is.null(.self$model.call$tau))
      .self$tau <- .self$model.call$tau
    else 
      .self$tau <- 0.5
    callSuper(formula=formula, data=data, ..., weights = NULL, by = by)
  }
)

zquantile$methods(
  param = function(z.out) {
    object <- z.out
    rq.sum <- summary.rq(object, cov = TRUE, se = object$se)
    return(mvrnorm(n = .self$num, mu = object$coef, Sigma = rq.sum$cov))
  }
)

zls$methods(
  qi = function(simparam, mm) {
    ev <- simparam %*% t(mm)
    pv <- ev
    return(list(ev = ev, pv = pv))
  }
)

zquantile$methods(
  qi = function(simparam, mm) {
    object <- .self$zelig.out[[i]]
    coeff <- simparam
    ev <- coeff %*% t(x)
    pv <- ev
    n <- nrow(.self$data)
    h <- bandwidth.rq(.self$tau, n) # estimate optimal bandwidth for sparsity
    if (.self$tau + h > 1)
      stop("tau + h > 1. Sparsity estimate failed. Please specify a tau closer to 0.5")
    if (.self$tau - h < 0)
      stop("tau - h < 0. Sparsity estimate failed. Please specify a tau closer to 0.5")
    beta_high <- rq(object$formula, data = .self$data, tau = .self$tau + h )$coef
    beta_low <- rq(object$formula, data = .self$data, tau = .self$tau - h)$coef
    F_diff <- x %*% (beta_high - beta_low)
    if (any(F_diff <= 0))
      warning(paste(sum(F_diff <= 0),
                    "density estimates were non-positive. Predicted values will likely be non-sensical."))
    # Includes machine error correction as per summary.rq for nid case
    f <- pmax(0, (2 * h) / (F_diff - eps))
    # Use asymptotic approximation of Q(tau|X,beta) distribution
    for(ii in 1:nrow(ev))
      # Asymptotic distribution as per Koenker 2005 _Quantile Regression_ p. 72
      pv[ii, ] <- rnorm(length(ev[ii, ]), mean=ev[ii, ],
                        sqrt((.self$tau * (1 - .self$tau))) / (f * sqrt(n)))
    return(list(ev  = ev, pv = pv))
  }
)
