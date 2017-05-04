#' Instrumental-Variable Regression
#'
#' Fit instrumental-variable regression by two-stage least squares. This is
#' equivalent to direct instrumental-variables estimation when the number of
#' instruments is equal to the number of predictors.
#'
#' @param formula specification(s) of the regression relationship
#' @param instruments the instruments. Either `instruments` is missing and
#'   formula has three parts as in `y ~ x1 + x2 | z1 + z2 + z3` (recommended) or
#'   formula is `y ~ x1 + x2` and instruments is a one-sided formula
#' `~ z1 + z2 + z3`. Using `instruments` is not recommended with `zelig`.
# @param an optional list. See the `contrasts.arg` of
#   \code{\link{model.matrix.default}}.
#' @param model,x,y logicals. If `TRUE` the corresponding components of the fit
#' (the model frame, the model matrices , the response) are returned.
#' @param ... further arguments passed to methods. See also \code{\link{zelig}}.
#'
#' @details Regressors and instruments for `ivreg` are most easily specified in
#'   a formula with two parts on the right-hand side, e.g.,
#'   `y ~ x1 + x2 | z1 + z2 + z3`, where `x1` and `x2` are the regressors and
#'   `z1`, `z2`, and `z3` are the instruments. Note that exogenous regressors
#'   have to be included as instruments for themselves. For example, if there is
#'   one exogenous regressor `ex` and one endogenous regressor `en` with
#'   instrument `in`, the appropriate formula would be `y ~ ex + en | ex + in`.
#'   Equivalently, this can be specified as `y ~ ex + en | . - en + in`, i.e.,
#'   by providing an update formula with a `.` in the second part of the
#'   formula. The latter is typically more convenient, if there is a large
#'   number of exogenous regressors.
#'
#' @examples
#' library(AER) # for sandwich vcov
#' library(dplyr) # for the pipe operator %>%
#'
#' # load and transform data
#' data("CigarettesSW")
#' CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
#' CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
#' CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)
#'
#' # log second stage independent variables, as logging internally for ivreg is
#' # not currently supported
#' CigarettesSW$log_rprice <- log(CigarettesSW$rprice)
#' CigarettesSW$log_rincome <- log(CigarettesSW$rincome)
#'
#' # estimate model
#' z.out1 <- zelig(log(packs) ~ log_rprice + log_rincome |
#'                     log_rincome + tdiff + I(tax/cpi),
#'                     data = CigarettesSW, subset = year == "1995",
#'                     model = "ivreg")
#' summary(z.out1)
#' from_zelig_model(z.out1) %>% summary(vcov = sandwich, df = Inf,
#'                                      diagnostics = TRUE)
#' # simulate and plot quantities of interest
#' z.out1 %>% setx() %>% sim() %>% plot()
#'
#' # ANOVA
#' z.out2 <- zelig(log(packs) ~ log_rprice |
#'                 tdiff, data = CigarettesSW, subset = year == "1995",
#'                 model = "ivreg")
#' anova(from_zelig_model(z.out1), from_zelig_model(z.out2))
#'
#' @source `ivreg` is from Christian Kleiber and Achim Zeileis (2008). Applied
#' Econometrics with R. New York: Springer-Verlag. ISBN 978-0-387-77316-2. URL
#' <https://CRAN.R-project.org/package=AER>
#'
#' @seealso \code{\link{zelig}},
#' Greene, W. H. (1993) *Econometric Analysis*, 2nd ed., Macmillan.
#'
#' @md
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
#    mcfun = function(z, h, b0 = 0, b1 = 1, alpha = 1, sim = TRUE){
#        x <- b0 + 2*z + 3*h + sim * rnorm(n = length(z), sd = alpha + 1)
#        y <- b0 + b1*x + sim * rnorm(n = length(z), sd = alpha)
#        yx <- list(y, x)
#        return(yx)
#    }
#)
