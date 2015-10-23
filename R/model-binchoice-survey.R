#' @include model-zelig.R
#' @include model-survey.R
#' @include model-binchoice.R

zbinchoice <- setRefClass("Zelig-binchoice-survey",
                          contains = c("Zelig-survey",
                                     "Zelig-binchoice"))
  
zbinchoice$methods(
  initialize = function() {
    callSuper()
    .self$family <- "binomial"
    .self$year <- 2015
    .self$category <- "dichotomous"
    # JSON from parent
    .self$outcome <- "binary"
  }
)

zbinchoice$methods(
  param = function(z.out) {
    simparam.local <- callSuper(z.out)
    return(simparam.local$simparam) # no ancillary parameter
  }
)
