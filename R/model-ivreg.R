#' Instrumental-Variable Regression
#'@param formula a symbolic representation of the model to be
#'   estimated, in the form \code{y \~\, x1 + x2}, where \code{y} is the
#'   dependent variable and \code{x1} and \code{x2} are the explanatory
#'   variables, and \code{y}, \code{x1}, and \code{x2} are contained in the
#'   same dataset. (You may include more than two explanatory variables,
#'   of course.) The \code{+} symbol means ``inclusion'' not
#'   ``addition.'' You may also include interaction terms and main
#'   effects in the form \code{x1*x2} without computing them in prior
#'   steps; \code{I(x1*x2)} to include only the interaction term and
#'   exclude the main effects; and quadratic terms in the form
#'   \code{I(x1^2)}.
#'@param model the name of a statistical model to estimate.
#'   For a list of supported models and their documentation see:
#'   \url{http://docs.zeligproject.org/articles/}.
#'@param data the name of a data frame containing the variables
#'   referenced in the formula or a list of multiply imputed data frames
#'   each having the same variable names and row numbers (created by
#'   \code{Amelia} or \code{\link{to_zelig_mi}}).
#'@param ... additional arguments passed to \code{zelig},
#'   relevant for the model to be estimated.
#'@param by a factor variable contained in \code{data}. If supplied,
#'   \code{zelig} will subset
#'   the data frame based on the levels in the \code{by} variable, and
#'   estimate a model for each subset. This can save a considerable amount of
#'   effort. For example, to run the same model on all fifty states, you could
#'   use: \code{z.out <- zelig(y ~ x1 + x2, data = mydata, model = 'ls',
#'   by = 'state')} You may also use \code{by} to run models using MatchIt
#'   subclasses.
#'@param cite If is set to 'TRUE' (default), the model citation will be printed
#'   to the console.
#'
#'@details
#' Additional parameters avaialable to many models include:
#' \itemize{
#'   \item weights: vector of weight values or a name of a variable in the dataset
#'   by which to weight the model. For more information see:
#'   \url{http://docs.zeligproject.org/articles/weights.html}.
#'   \item bootstrap: logical or numeric. If \code{FALSE} don't use bootstraps to
#'   robustly estimate uncertainty around model parameters due to sampling error.
#'   If an integer is supplied, the number of boostraps to run.
#'   For more information see:
#'   \url{http://docs.zeligproject.org/articles/bootstraps.html}.
#' }
#' @return Depending on the class of model selected, \code{zelig} will return
#'   an object with elements including \code{coefficients}, \code{residuals},
#'   and \code{formula} which may be summarized using
#'   \code{summary(z.out)} or individually extracted using, for example,
#'   \code{coef(z.out)}. See
#'   \url{http://docs.zeligproject.org/articles/getters.html} for a list of
#'   functions to extract model components. You can also extract whole fitted
#'   model objects using \code{\link{from_zelig_model}}.
#'
#'
#'
#'@examples
#'library(Zelig)
#'library(dplyr) # for the pipe operator %>%
#'load and transform data
#'data("CigarettesSW")
#'CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
#'CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
#'CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)
#'log second stage independent variables, as logging internally for ivreg is
#'not currently supported
#'CigarettesSW$log_rprice <- log(CigarettesSW$rprice)
#'CigarettesSW$log_rincome <- log(CigarettesSW$rincome)
#'z.out1 <- zelig(log(packs) ~ log_rprice + log_rincome |
#'log_rincome + tdiff + I(tax/cpi),data = CigarettesSW, subset = year == "1995",model = "ivreg")
#'summary(z.out1)
#' @seealso Vignette: \url{http://docs.zeligproject.org/articles/zelig_ivreg.html}
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
#' library(Zelig)
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
#'                     data = CigarettesSW,
#'                     model = "ivreg")
#' summary(z.out1)
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
