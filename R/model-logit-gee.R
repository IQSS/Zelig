zlogitgee <- setRefClass("Zelig-logit-gee",
                           contains = c("Zelig-binchoice-gee"))

zlogitgee$methods(
  initialize = function() {
    callSuper()
    .self$name <- "logit-gee"
    .self$link <- "logit"
    .self$description = "General Estimating Equation for Logistic Regression"
  }
)