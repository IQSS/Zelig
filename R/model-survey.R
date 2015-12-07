#' @include model-zelig.R
zsurvey <- setRefClass("Zelig-survey",
                    contains = "Zelig")

zsurvey$methods(
  initialize = function() {
    callSuper()
    .self$fn <- quote(survey::svyglm)
    .self$packageauthors <- "Thomas Lumley"
    .self$modelauthors <- "Nicholas Carnes"
    .self$acceptweights <- TRUE
  }
)

zsurvey$methods(
  zelig = function(formula, data, ids=~1, probs=NULL, strata=NULL, fpc=NULL, nest=FALSE, check.strata=!nest, ... , weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    design <- survey::svydesign(data= data, ids=~1, probs=probs, strata=strata, fpc=fpc, nest=nest, check.strata=check.strata)
    .self$model.call <- as.call(list(.self$fn, formula=.self$zelig.call$formula,  design=design))  # fn will be set again by super, but initialized here for clarity
    .self$model.call$family <- call(.self$family, .self$link)
    callSuper(formula = formula, data = data, ..., weights = weights, by = by)
  }
)

zsurvey$methods(
  param = function(z.out) {
    return(mvrnorm(.self$num, coef(z.out), vcov(z.out)))
  }
)
