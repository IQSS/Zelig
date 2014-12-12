zpoissongee <- setRefClass("Zelig-poisson-gee",
                           contains = c("Zelig-gee", "Zelig-poisson"))

zpoissongee$methods(
  initialize = function() {
    callSuper()
    .self$name <- "poisson-gee"
    .self$family <- "poisson"
    .self$link <- "log"
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$year <- 2011
    .self$category <- "continuous"
    .self$authors <- "Patrick Lam"
    .self$description = "General Estimating Equation for Poisson Regression"
    .self$fn <- quote(geepack::geeglm)
    # JSON from parent
    .self$wrapper <- "poisson.gee"
  }
)


zpoissongee$methods(
  param = function(z.out) {
    simparam <- callSuper(z.out)
    return(simparam$simparam) # no ancillary parameter
  }
)

