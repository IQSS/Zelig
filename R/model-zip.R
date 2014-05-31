zzip <- setRefClass("Zelig-zip", contains = "Zelig")

zzip$methods(
  initialize = function() {
    callSuper()
    .self$model <- "zip"
    .self$year <- 2014
    .self$authors <- "Vito d'Orazio, Christine Choirat"
    .self$category <- "continuous"
    .self$text = "Zero-inflated Count Data Regression"
    .self$fn <- quote(pscl::zeroinfl)
    # JSON
    .self$outcome <- "discrete"
  }
)

zzip$methods(
  zelig = function(formula, data, ..., weights = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    callSuper(formula = formula, data = data, ...,
              weights = NULL)
    .self$zelig.out <- eval(.self$model.call, envir = parent.frame(1))
  }
)


