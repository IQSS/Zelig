#' @include model-binchoice-gee.R
zprobitgee <- setRefClass("Zelig-probit-gee",
                          contains = c("Zelig-binchoice-gee"))

zprobitgee$methods(
  initialize = function() {
    callSuper()
    .self$name <- "probit-gee"
    .self$link <- "probit"
    .self$description <- "General Estimating Equation for Probit Regression"
    .self$wrapper <- "probit.gee"
  }
)