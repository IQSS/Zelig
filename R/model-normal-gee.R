znormalgee <- setRefClass("Zelig-normal-gee",
                           contains = c("Zelig-gee", "Zelig-normal"))

znormalgee$methods(
  initialize = function() {
    callSuper()
    .self$name <- "normal-gee"
    .self$family <- "gaussian"
    .self$link <- "identity"
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$year <- 2011
    .self$category <- "continuous"
    .self$authors <- "Patrick Lam"
    .self$description = "General Estimating Equation for Normal Regression"
    .self$fn <- quote(geepack::geeglm)
    # JSON from parent
    .self$wrapper <- "normal.gee"
  }
)
