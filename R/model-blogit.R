#' @include model-zelig.R
#' @include model-bbinchoice.R
zblogit <- setRefClass("Zelig-blogit", contains = "Zelig-bbinchoice")

zblogit$methods(
  initialize = function() {
    callSuper()
    .self$name <- "blogit"
    .self$description <- "Bivariate Logit Regression for Dichotomous Dependent Variables"
    .self$family <- quote(binom2.or(zero = 3))
    .self$linkinv <- binom2.or()@linkinv
  }
)
