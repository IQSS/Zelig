#' @include model-zelig.R
#' @include model-glm.R
#' @include model-binchoice.R
zlogit <- setRefClass("Zelig-logit",
                      contains = "Zelig-binchoice")
  
zlogit$methods(
  initialize = function() {
    callSuper()
    .self$name <- "logit"
    .self$link <- "logit"
    .self$description = "Logistic Regression for Dichotomous Dependent Variables"
  }
)

