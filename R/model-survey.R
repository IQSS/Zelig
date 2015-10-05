#' @include model-zelig.R
zsurvey <- setRefClass("Zelig-survey",
                    contains = "Zelig")

zsurvey$methods(
  initialize = function() {
    callSuper()
    .self$fn <- quote(survey::svyglm)
    .self$packageauthors <- "Thomas Lumley"
    #.self$modelauthors <- "Nicholas Carnes" ??
    .self$acceptweights <- TRUE
  }
)

zsurvey$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    .self$model.call$family <- call(.self$family, .self$link)
    callSuper(formula = formula, data = data, ..., weights = weights, by = by)
  }
)

zsurvey$methods(
  param = function(z.out) {
    return(mvrnorm(.self$num, coef(z.out), vcov(z.out)))
  }
)
