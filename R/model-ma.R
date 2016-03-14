#' Time-Series Model with Moving Average
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-ma.html}
#' @import methods
#' @export Zelig-ma
#' @exportClass Zelig-ma
#'
#' @include model-zelig.R
#' @include model-timeseries.R
  
zma <- setRefClass("Zelig-ma",
                       contains = "Zelig-timeseries")

zma$methods(
  initialize = function() {
    callSuper()
    .self$name <- "ma"
    .self$link <- "identity"
    .self$fn <- quote(zeligArimaWrapper)
    .self$description = "Time-Series Model with Moving Average"
    .self$packageauthors <- "R Core Team"
    .self$outcome <- "continuous"
    .self$wrapper <- "timeseries"
  }
)
