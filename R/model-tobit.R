ztobit <- setRefClass("Zelig-tobit",
                      contains = "Zelig",
                      fields = list(simalpha = "matrix",
                                    linkinv = "function"))

ztobit$methods(
  initialize = function() {
    callSuper()
    .self$model <- "tobit"
    .self$authors <- "Kosuke Imai, Gary King, Olivia Lau"
    .self$year <- 2011
    .self$text = "Linear regression for Left-Censored Dependent Variable"
    .self$fn <- quote(survival::survreg)
#     .self$linkinv <- survreg.distributions[["gaussian"]]$itrans
  }
)

ztobit$methods(
  zelig = function(formula, ..., below = 0, above = Inf, robust = FALSE, cluster = NULL, data) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    if (!(is.null(cluster) || robust))
      stop("If cluster is specified, then `robust` must be TRUE")
    # Add cluster term
    if (robust || !is.null(cluster))
      formula <- cluster.formula(formula, cluster)
    # Make surv demands that the model 
    formula <- make.surv(formula, below, above)
    formula <- cluster.formula(formula, cluster)
    callSuper(formula = formula, data = data, ..., robust = robust, cluster = cluster)
    .self$model.call$dist <- "gaussian"
    .self$model.call$model <- FALSE
    .self$model.call$formula <- formula
    .self$zelig.out <- eval(.self$model.call)
  }
)

ztobit$methods(
  param = function(num) {
    mu <- c(coef(.self$zelig.out), log(.self$zelig.out$scale))
    cov <- vcov(.self$zelig.out)
    .self$simparam = mvrnorm(num, mu=mu, Sigma=cov)
  }
)

ztobit$methods(
  qi = function(x) {
    # This needs to be fixed.
    ev <- pr <- NA
    return(list("Expected Values: E(Y|X)"  = ev,
                "Predicted Values: Y|X"    = pr)
    )
  }
)

make.surv <- function (formula, below, above) {
  lhs <- formula[[2]]
  if (grepl("Surv", as.character(lhs)))
    return(formula)
  if (!(is.numeric(below) && is.numeric(above))) {
    warning("`below` and `above` must be numeric; ",
            "returning the original formula")
    return(formula)
  }
  if (above == Inf) {
    # Empty?
    # This seems like a mistake inherited from old code
  }
  else if (below == -Inf && above == Inf)
    stop("This model does not support censoring. Try the \"normal\" model")
  else if (below == -Inf && above != Inf)
    stop("This model does not support right-censored data")
  else if (is.finite(below) && is.finite(above))
    stop("This model does not support interval-censored data")
  # That is, this model only supports left-censored data
  # Surv( <outcome> , <below> < <outcomes> )
  lhs <- call("Surv", lhs, call("<", below, lhs), type="left")
  # Place back within formula
  formula[[2]] <- lhs
  return(formula)
}
