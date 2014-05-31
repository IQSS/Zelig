zls <- setRefClass("Zelig-ls", contains = "Zelig")

zls$methods(
  initialize = function() {
    callSuper()
    .self$model <- "ls"
    .self$year <- 2007
    .self$category <- "continuous"
    .self$text = "Least Squares Regression for Continuous Dependent Variables"
    .self$fn <- quote(stats::lm)
    # JSON
    .self$outcome <- "continous"
  }
)

zls$methods(
  zelig = function(formula, data, ..., weights = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    callSuper(formula = formula, data = data, ...,
              weights = NULL)
    .self$zelig.out <- eval(.self$model.call, envir = parent.frame(1))
#     .self$zelig.out <- eval(.self$model.call, envir = .self$envir)
#     .self$zelig.out <- eval(.self$model.call)
  }
)

zls$methods(
  param = function(num) {
    .self$simparam <- mvrnorm(n = num,
                              mu = coef(.self$zelig.out),
                              Sigma = vcov(.self$zelig.out))
  }
)

zls$methods(
  ev = function(which) {
    if (which %in% c("x", "x1"))
      res <- .self$simparam %*% t(.self$setx.out[[which]])
    else if (which == "range") {
      res <- list()
      for (i in seq(.self$setx.out$range))
        res[[i]] <- .self$simparam %*% t(.self$setx.out$range[[i]])
    }
    return(res)
  }
)

zls$methods(
  pv = function(which) {
    if (which %in% c("x", "x1"))
      res <- .self$simparam %*% t(.self$setx.out[[which]])
    else if (which == "range") {
      res <- list()
      for (i in seq(.self$setx.out$range))
        res[[i]] <- .self$simparam %*% t(.self$setx.out$range[[i]])
    }
    return(res)
  }
)

# zls$methods(
#   qi = function() {
# #     .self$qi.out$ev <- .self$simparam %*% t(.self$setx.out$x)
# #     .self$qi.out$pv <- .self$qi.out$ev
# #     if (!is.null(.self$setx.out$x1)) {
# #       .self$qi.out$ev1 <- .self$simparam %*% t(.self$setx.out$x1)
# #       .self$qi.out$pv1 <- .self$qi.out$pv
# #       .self$qi.out$fd <- .self$qi.out$ev1 - .self$qi.out$ev
# #     }
#     callSuper()
#   }
# )
