#' Time-Series Model with Autoregressive Disturbance
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-ar.html}
#' @import methods
#' @export Zelig-ar
#' @exportClass Zelig-ar
#'
#' @include model-zelig.R
#' @include model-timeseries.R
  
zar <- setRefClass("Zelig-ar",
                       contains = "Zelig-timeseries")

zar$methods(
  initialize = function() {
    callSuper()
    .self$name <- "ar"
    .self$link <- "identity"
    .self$fn <- quote(zeligArimaWrapper)
    .self$description = "Time-Series Model with Autoregressive Disturbance"
    .self$packageauthors <- "R Core Team"
    .self$outcome <- "continuous"
    .self$wrapper <- "timeseries"
  }
)
