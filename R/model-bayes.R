#' @include model-zelig.R
zbayes <- setRefClass("Zelig-bayes",
                      contains = "Zelig")

zbayes$methods(
  initialize = function() {
    callSuper()
    .self$packageauthors <- "Andrew D. Martin, Kevin M. Quinn, and Jong Hee Park"
    .self$modelauthors <- "Ben Goodrich, and Ying Lu"
  }
)

zbayes$methods(
  zelig = function(formula, 
                   burnin = 1000, mcmc = 10000, 
                   verbose = 0, 
                   ..., 
                   data,
                   by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    if (missing(verbose))
      verbose <- round((mcmc + burnin) / 10)
#     .self$model.call$family <- call(.self$family, .self$link)
    .self$model.call$verbose <- verbose
    .self$num <- mcmc # CC: check
    callSuper(formula = formula, data = data, ..., by = by)
  }
)

zbayes$methods(
  param = function(z.out) {
    return(z.out)
  }
)
