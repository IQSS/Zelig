#' Time-Series Model with Autoregressive Disturbance
#'
#'
#'
#' @param formula a symbolic representation of the model to be
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
#' @param model the name of a statistical model to estimate.
#'   For a list of supported models and their documentation see:
#'   \url{http://docs.zeligproject.org/articles/}.
#' @param data the name of a data frame containing the variables
#'   referenced in the formula or a list of multiply imputed data frames
#'   each having the same variable names and row numbers (created by
#'   \code{Amelia} or \code{\link{to_zelig_mi}}).
#' @param ... additional arguments passed to \code{zelig},
#'   relevant for the model to be estimated.
#' @param by a factor variable contained in \code{data}. If supplied,
#'   \code{zelig} will subset
#'   the data frame based on the levels in the \code{by} variable, and
#'   estimate a model for each subset. This can save a considerable amount of
#'   effort. For example, to run the same model on all fifty states, you could
#'   use: \code{z.out <- zelig(y ~ x1 + x2, data = mydata, model = 'ls',
#'   by = 'state')} You may also use \code{by} to run models using MatchIt
#'   subclasses.
#' @param cite If is set to 'TRUE' (default), the model citation will be printed
#'   to the console.
#' @param ts The name of the variable containing the time indicator. This should be passed in as
#'     a string. If this variable is not provided, Zelig will assume that the data is already
#'     ordered by time.
#' @param cs Name of a variable that denotes the cross-sectional element of the data, for example,
#'  country name in a dataset with time-series across different countries. As a variable name,
#'  this should be in quotes. If this is not provided, Zelig will assume that all observations
#'  come from the same unit over time, and should be pooled, but if provided, individual models will
#'  be run in each cross-section.
#'  If \code{cs} is given as an argument, \code{ts} must also be provided. Additionally, \code{by}
#'  must be \code{NULL}.
#' @param order A vector of length 3 passed in as \code{c(p,d,q)} where p represents the order of the
#'     autoregressive model, d represents the number of differences taken in the model, and q represents
#'     the order of the moving average model.
#' @details
#' Currently only the Reference class syntax for time series. This model does not accept
#' Bootstraps or weights.
#'
#'
#' @return Depending on the class of model selected, \code{zelig} will return
#'   an object with elements including \code{coefficients}, \code{residuals},
#'   and \code{formula} which may be summarized using
#'   \code{summary(z.out)} or individually extracted using, for example,
#'   \code{coef(z.out)}. See
#'   \url{http://docs.zeligproject.org/articles/getters.html} for a list of
#'   functions to extract model components. You can also extract whole fitted
#'   model objects using \code{\link{from_zelig_model}}.
#' @examples
#' data(seatshare)
#' subset <- seatshare[seatshare$country == "UNITED KINGDOM",]
#' ts.out <- zar$new()
#' ts.out$zelig(formula = unemp ~ leftseat, order = c(1,0,1), ts = "year", data = subset)
#' summary(ts.out)
#'
#' @seealso Vignette: \url{http://docs.zeligproject.org/articles/zelig_ar.html}
#'
#' @import methods
#' @export Zelig-ar
#' @exportClass Zelig-ar
#'
#' @include model-zelig.R
#' @include model-timeseries.R
zar <- setRefClass("Zelig-ar",
                       contains = "Zelig-timeseries")

zar$methods(
  initialize = function() {
    callSuper()
    .self$name <- "ar"
    .self$link <- "identity"
    .self$fn <- quote(zeligArimaWrapper)
    .self$description = "Time-Series Model with Autoregressive Disturbance"
    .self$packageauthors <- "R Core Team"
    .self$outcome <- "continuous"
    .self$wrapper <- "timeseries"
  }
)
