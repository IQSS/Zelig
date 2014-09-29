zprobitgee <- setRefClass("Zelig-probit-gee",
                          contains = c("Zelig-binchoice-gee"))

zprobitgee$methods(
  initialize = function() {
    callSuper()
    .self$name <- "probitgee"
    .self$link <- "probit"
    .self$description = "General Estimating Equation for Probit Regression"
  }
)