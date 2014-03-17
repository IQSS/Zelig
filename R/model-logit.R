zlogit <- setRefClass("Zelig-logit",
                      contains="Zelig-binchoice")
  
zlogit$methods(
  initialize = function() {
    callSuper()
    .self$model <- "logit"
    .self$link <- "logit"
    .self$text = "Logistic Regression for Dichotomous Dependent Variables"
  }
)

