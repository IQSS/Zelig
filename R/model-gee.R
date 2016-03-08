#' Generalized Estimating Equations Model object for inheritance across models in Zelig
#'
#' @import methods
#' @export Zelig-gee
#' @exportClass Zelig-gee
#'
#' @include model-zelig.R

zgee <- setRefClass("Zelig-gee",
                    contains = "Zelig")

zgee$methods(
  initialize = function() {
    callSuper()
    .self$packageauthors <- "Soren Hojsgaard, Ulrich Halekoh, and Jun Yan"
    .self$modelauthors <- "Patrick Lam"
    .self$acceptweights <- TRUE
  }
)


zgee$methods(
  zelig = function(formula, id, ..., zcor = NULL, corstr = "independence", data, weights = NULL, by = NULL, bootstrap = FALSE) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    if (corstr == "fixed" && is.null(zcor))
      stop("R must be defined")
    # if id is a valid column-name in data, then we just need to extract the
    # column and re-order the data.frame and cluster information
    if (is.character(id) && length(id) == 1 && id %in% colnames(data)) {
      id <- data[, id]
      data <- data[order(id), ]
      id <- sort(id)
    }
    .self$model.call$family <- call(.self$family, .self$link)
    .self$model.call$id <- id
    .self$model.call$zcor <- zcor
    .self$model.call$corstr <- corstr
    callSuper(formula = formula, data = data, ..., weights = weights, by = by, bootstrap = bootstrap)
    # Prettify summary display without modifying .self$model.call
    for (i in length(.self$zelig.out$z.out)) {
      .self$zelig.out$z.out[[i]]$call$id <- .self$zelig.call$id
      .self$zelig.out$z.out[[i]]$call$zcor <- "zcor"
    }
  }
)
   
zgee$methods(
  param = function(z.out, method="mvn") {
    so <- summary(z.out)
    shape <- so$dispersion
    if(identical(method,"point")){
      return( list(simparam = t(as.matrix(coef(z.out))), simalpha = shape[1][1] ))
    }else if(identical(method,"mvn")){
      simalpha <- rnorm(n = .self$num,
                      mean = shape[1][[1]],
                      sd = shape[2][[1]])
      simparam.local <- mvrnorm(n = .self$num,
                        mu = coef(z.out),
                        Sigma = so$cov.unscaled)
      simparam.local <- list(simparam = simparam.local, simalpha = simalpha)
      return(simparam.local)
    }
  }
)

# zgee$methods(
#   show = function() {
#     for (i in length(.self$zelig.out$z.out)) {
#       .self$zelig.out$z.out[[i]]$call$id <- "id"
#     }
#     callSuper()
#   }
# )
