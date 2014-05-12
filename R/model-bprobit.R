zbprobit <- setRefClass("Zelig-bprobit", contains = "Zelig-bbinchoice")

zbprobit$methods(
  initialize = function() {
    callSuper()
    .self$model <- "bprobit"
    .self$text <- "Bivariate Probit Regression for Dichotomous Dependent Variables"
    .self$family <- quote(binom2.rho(zero = 3))
    .self$linkinv <- binom2.rho()@linkinv
  }
)

