#' @include model-zelig.R
zls <- setRefClass("Zelig-ls", contains = "Zelig")

zls$methods(
  initialize = function() {
    callSuper()
    .self$name <- "ls"
    .self$year <- 2007
    .self$category <- "continuous"
    .self$description <- "Least Squares Regression for Continuous Dependent Variables"
    .self$fn <- quote(stats::lm)
    # JSON
    .self$outcome <- "continous"
  }
)

zls$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)
    callSuper(formula = formula, data = data, ...,
              weights = NULL, by = by)
  }
)

# zls$methods(
#   zelig = function(formula, data, ..., weights = NULL) {
#     .self$zelig.call <- match.call(expand.dots = TRUE)
#     .self$formula <- formula
#     .self$data <- data
#     .self$model.call <- match.call(expand.dots = TRUE)
#     .self$model.call[[1]] <- .self$fn
#     .self$model.call$by <- NULL
#     .self$zelig.out <- eval(.self$model.call)
#     if (!is.null(by)) {
#       .self$data.by <- split(.self$data, factor(.self$data[[by]]))
#       .self$zelig.out.by <- list()
#       for (i in seq(.self$data.by)) {
#         model.call.by <- .self$model.call
#         model.call.by$data <- quote(.self$data.by[[i]]) # names(z5$data.by)
#         .self$zelig.out.by[[i]] <- eval(model.call.by)
#       }
#     }
#   }
# )

zls$methods(
  param = function(num) {
    .self$simparam <- mvrnorm(n = num,
                              mu = coef(.self$zelig.out),
                              Sigma = vcov(.self$zelig.out))
  }
)

zls$methods(
  qi = function(x) {
    ev <- .self$simparam %*% t(x)
    pv <- ev
    return(list(ev = ev, pv = pv))
  }
)
