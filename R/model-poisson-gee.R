zpoissongee <- setRefClass("Zelig-poisson-gee", contains = "Zelig")

zpoissongee$methods(
  initialize = function() {
    callSuper()
    .self$model <- "poissongee"
    .self$year <- 2011
    .self$category <- "continuous"
    .self$authors <- "Patrick Lam"
    .self$text = "General Estimating Equation for Poisson Regression"
    .self$fn <- quote(gee::gee)
  }
)

zpoissongee$methods(
  zelig = function(formula, id, ..., R = NULL, corstr = "independence", data) {
    if (corstr == "fixed" && is.null(R))
      stop("R must be defined")
    # if id is a valid column-name in data, then we just need to extract the
    # column and re-order the data.frame and cluster information
    if (is.character(id) && length(id) == 1 && id %in% colnames(data)) {
      id <- data[, id]
      data <- data[order(id), ]
      id <- sort(id)
    }
    .self$zelig.call <- match.call(expand.dots = TRUE)
    callSuper(formula=formula, data=data, ...)
    .self$model.call$family  = quote(poisson())
    .self$model.call$id <- id
    .self$model.call$R <- R
    .self$model.call$corstr <- corstr
    .self$zelig.out <- eval(.self$model.call)
  }
)

zpoissongee$methods(
  param = function(num) {
    .self$simparam <- mvrnorm(n=num, mu=.self$zelig.out$coefficients,
                              Sigma=.self$zelig.out$naive.variance)
  }
)

zpoissongee$methods(
  qi = function(x) {
    coef <- .self$simparam
    eta <- coef %*% t(x)
    theta <- matrix(poisson()$linkinv(eta), nrow = nrow(coef))
    # To mimick Zelig 4 (incorrect behavior of "inverse" function):
    # theta <- matrix(1 / eta, nrow = nrow(coef))
    ev <- theta
    return(list("Expected Values: E(Y|X)"  = ev))
  }
)
