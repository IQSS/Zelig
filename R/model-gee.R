#' @include model-zelig.R
zgee <- setRefClass("Zelig-gee",
                    contains = "Zelig")

zgee$methods(
  initialize = function() {
    callSuper()
    .self$packageauthors <- "Søren Højsgaard, Ulrich Halekoh, and Jun Yan"
  }
)


zgee$methods(
  zelig = function(formula, id, ..., zcor = NULL, corstr = "independence", data, by = NULL) {
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
    callSuper(formula = formula, data = data, ..., by = by)
    # Prettify summary display without modifying .self$model.call
    for (i in length(.self$zelig.out$z.out))
      .self$zelig.out$z.out[[i]]$call$id <- .self$zelig.call$id
  }
)

zgee$methods(
  param = function(z.out, ancillary = TRUE) {
    so <- summary(z.out)
    shape <- so$dispersion
    simalpha <- rnorm(n = .self$num,
                      mean = shape[1][[1]],
                      sd = shape[2][[1]])
    simparam <- mvrnorm(n = .self$num,
                        mu = coef(z.out),
                        Sigma = so$cov.unscaled)
    simparam <- list(simparam = simparam, simalpha = simalpha)
    return(simparam)
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
