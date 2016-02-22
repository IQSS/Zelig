#' @include model-zelig.R
ztimeseries <- setRefClass("Zelig-timeseries",
                    contains = "Zelig",
                    fields = list(link = "character",
                                  linkinv = "function"))

ztimeseries$methods(
  initialize = function() {
    callSuper()
    .self$packageauthors <- ""  # Need to decide
    .self$acceptweights <- FALSE  #  Need to deal with block bootstrap
  }
)

ztimeseries$methods(
  zelig = function(formula, data, ..., weights=NULL, by=NULL){
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    callSuper(formula = formula, data = data, ..., weights = weights, by = by)
  }
)

ztimeseries$methods(
  param = function(z.out) {
    return(mvrnorm(.self$num, coef(z.out), vcov(z.out)))
  }
)
