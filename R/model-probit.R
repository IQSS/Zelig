#' @include model-zelig.R
#' @include model-glm.R
#' @include model-binchoice.R
zprobit <- setRefClass("Zelig-probit",
                       contains = "Zelig-binchoice")

zprobit$methods(
  initialize = function() {
    callSuper()
    .self$name <- "probit"
    .self$link <- "probit"
    .self$description = "Probit Regression for Dichotomous Dependent Variables"
  }
)

