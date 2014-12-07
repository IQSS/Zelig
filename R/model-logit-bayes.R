#' @include model-zelig.R
#' @include model-bayes.R

zlogitbayes <- setRefClass("Zelig-logit-bayes",
                             contains = c("Zelig-bayes",
                                          "Zelig-logit"))

zlogitbayes$methods(
  initialize = function() {
    callSuper()
    .self$name <- "logitbayes"
    .self$family <- "binomial"
    .self$link <- "logit"
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$year <- 2013
    .self$category <- "dichotomous"
    .self$authors <- "Ben Goodrich, Ying Lu"
    .self$description = "Bayesian Logistic Regression for Dichotomous Dependent Variables"
    .self$fn <- quote(MCMCpack::MCMClogit)
    # JSON from parent
    .self$wrapper <- "logit.bayes"
  }
)
