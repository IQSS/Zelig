zbinchoicegee <- setRefClass("Zelig-binchoice-gee",
                           contains = c("Zelig-gee",
                                        "Zelig-binchoice"))

zbinchoicegee$methods(
  initialize = function() {
    callSuper()
    .self$family <- "binomial"
    .self$year <- 2011
    .self$category <- "continuous"
    .self$authors <- "Patrick Lam"
    .self$description = "General Estimating Equation for Logistic Regression"
    .self$fn <- quote(geepack::geeglm)
    # JSON from parent
  }
)

zbinchoicegee$methods(
  param = function(z.out) {
    simparam.local <- callSuper(z.out)
    return(simparam.local$simparam) # no ancillary parameter
  }
)
