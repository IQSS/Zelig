#' Instrumental-Variable Regression
#'
#' Fit instrumental-variable regression by two-stage least squares. This is
#' equivalent to direct instrumental-variables estimation when the number of
#' instruments is equal to the number of predictors.
#'
#' [FILL IN PARAMS]
#'
#' @import methods
#' @export Zelig-ivreg
#' @exportClass Zelig-ivreg
#'
#' @include model-zelig.R

zivreg <- setRefClass("Zelig-ivreg", contains = "Zelig")

zivreg$methods(
    initialize = function() {
        callSuper()
        .self$name <- "ivreg"
        .self$authors <- "Christopher Gandrud"
        .self$packageauthors <- "Christian Kleiber and Achim Zeileis"
        .self$year <- 2008
        .self$description <- "Instrumental-Variable Regression"
        .self$fn <- quote(AER::ivreg)
        # JSON
        .self$outcome <- "continous"
        .self$wrapper <- "ivreg"
        .self$acceptweights <- TRUE
    }
)

zivreg$methods(
    zelig = function(formula, data, ..., weights = NULL, by = NULL,
                     bootstrap = FALSE) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    callSuper(formula = formula, data = data, ...,
              weights = weights, by = by, bootstrap = bootstrap)

    # Automated Background Test Statistics and Criteria
    rse <- lapply(.self$zelig.out$z.out, (function(x) vcovHC(x, type = "HC0")))
    rse.se <- sqrt(diag(rse[[1]]))                 # Needs to work with "by" argument
    est.se <- sqrt(diag(.self$get_vcov()[[1]]))
  }
)

zivreg$methods(
    param = function(z.out, method = "mvn") {
        if(identical(method,"mvn")){
            return(list(simparam = mvrnorm(.self$num, coef(z.out), vcov(z.out)),
                   simalpha = rep(summary(z.out)$sigma, .self$num) )  )
        } else if(identical(method, "point")){
            return(list(simparam = t(as.matrix(coef(z.out))),
                        simalpha = summary(z.out)$sigma))
        } else {
            stop("param called with method argument of undefined type.")
        }
    }
)

zivreg$methods(
    qi = function(simparam, mm) {
        ev <- simparam$simparam %*% t(mm)
        pv <- as.matrix(rnorm(n = length(ev), mean = ev,
                              sd = simparam$simalpha), nrow = length(ev),
                              ncol = 1)
        return(list(ev = ev, pv = pv))
    }
)

#zivreg$methods(
#  mcfun = function(x, b0 = 0, b1 = 1, alpha = 1, sim = TRUE){
#    y <- b0 + b1*x + sim * rnorm(n = length(x), sd = alpha)
#    return(y)
#  }
#)
