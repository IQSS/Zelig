#' Quantile Regression for Continuous Dependent Variables
#'@param formula a symbolic representation of the model to be
#'   estimated, in the form \code{y ~ x1 + x2}, where \code{y} is the
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
#' @details
#' In addition to the standard inputs, \code{zelig} takes the following additional options
#' for quantile regression:
#' \itemize{
#'     \item \code{tau}: defaults to 0.5. Specifies the conditional quantile(s) that will be
#'     estimated. 0.5 corresponds to estimating the conditional median, 0.25 and 0.75 correspond
#'     to the conditional quartiles, etc. tau vectors with length greater than 1 are not currently
#'     supported. If tau is set outside of the interval [0,1], zelig returns the solution for all
#'     possible conditional quantiles given the data, but does not support inference on this fit
#'     (setx and sim will fail).
#'     \item \code{se}: a string value that defaults to "nid". Specifies the method by which
#'     the covariance matrix of coefficients is estimated during the sim stage of analysis. \code{se}
#'     can take the following values, which are passed to the \code{summary.rq} function from the
#'     \code{quantreg} package. These descriptions are copied from the \code{summary.rq} documentation.
#'     \itemize{
#'         \item \code{"iid"} which presumes that the errors are iid and computes an estimate of
#'         the asymptotic covariance matrix as in KB(1978).
#'         \item \code{"nid"} which presumes local (in tau) linearity (in x) of the the
#'         conditional quantile functions and computes a Huber sandwich estimate using a local
#'         estimate of the sparsity.
#'         \item \code{"ker"} which uses a kernel estimate of the sandwich as proposed by Powell(1990).
#'     }
#'     \item \code{...}: additional options passed to rq when fitting the model. See documentation for rq in the quantreg package for more information.
#' }
#' Additional parameters avaialable to this model include:
#' \itemize{
#'   \item \code{weights}: vector of weight values or a name of a variable in the dataset
#'   by which to weight the model. For more information see:
#'   \url{http://docs.zeligproject.org/articles/weights.html}.
#'   \item \code{bootstrap}: logical or numeric. If \code{FALSE} don't use bootstraps to
#'   robustly estimate uncertainty around model parameters due to sampling error.
#'   If an integer is supplied, the number of boostraps to run.
#'   For more information see:
#'   \url{http://docs.zeligproject.org/articles/bootstraps.html}.
#' }

#'
#' @return Depending on the class of model selected, \code{zelig} will return
#'   an object with elements including \code{coefficients}, \code{residuals},
#'   and \code{formula} which may be summarized using
#'   \code{summary(z.out)} or individually extracted using, for example,
#'   \code{coef(z.out)}. See
#'   \url{http://docs.zeligproject.org/articles/getters.html} for a list of
#'   functions to extract model components. You can also extract whole fitted
#'   model objects using \code{\link{from_zelig_model}}.
#'
#' @examples
#' library(Zelig)
#' data(stackloss)
#' z.out1 <- zelig(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.,
#' model = "rq", data = stackloss,tau = 0.5)
#' summary(z.out1)
#'
#' @seealso Vignette: \url{http://docs.zeligproject.org/articles/zelig_quantile.html}
#' @import methods
#' @export Zelig-quantile
#' @exportClass Zelig-quantile
#'
#' @include model-zelig.R

zquantile <- setRefClass("Zelig-quantile",
                         contains = "Zelig",
                         field = list(tau = "ANY"
                         ))

zquantile$methods(
  initialize = function() {
    callSuper()
    .self$fn <- quote(quantreg::rq)
    .self$name <- "quantile"
    .self$authors <- "Alexander D'Amour"
    .self$packageauthors <- "Roger Koenker"
    .self$modelauthors <- "Alexander D'Amour"
    .self$year <- 2008
    .self$category <- "continuous"
    .self$description <- "Quantile Regression for Continuous Dependent Variables"
    # JSON
    .self$outcome <- "continuous"
    .self$wrapper <- "rq"
    .self$acceptweights <- TRUE
  }
)

zquantile$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL,
                   bootstrap = FALSE) {

    # avoids CRAN warning about deep assignment from formula existing separately as argument and field
    localBy <- by
    # avoids CRAN warning about deep assignment from formula existing separately as argument and field
    localData <- data
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)

    if (!is.null(.self$model.call$tau)) {
        if (length(eval(.self$model.call$tau)) > 1) {
            stop('tau argument only accepts 1 value.\nZelig is using only the first value.',
                    call. = FALSE)
        } else
            .self$tau <- eval(.self$model.call$tau)
#        if (length(.self$tau) > 1) {
#            localData <- bind_rows(lapply(eval(.self$tau),
#                                      function(tau) cbind(tau, localData)))
#          #  localBy <- cbind("tau", localBy)
#        }
    } else
        .self$tau <- 0.5

    callSuper(formula = formula, data = localData, ..., weights = weights,
                by = localBy, bootstrap = bootstrap)

    rq_summaries <- lapply(.self$zelig.out$z.out, (function(x)
                            summary(x, se = "nid", cov = TRUE)))

    if (length(rq_summaries) > 1) {
        rse <- lapply(rq_summaries, function(y) y$cov)
    }
    else rse <- rq_summaries$cov
#    rse <- lapply(.self$zelig.out$z.out, (function(x)
#        quantreg::summary.rq(x, se = "nid", cov = TRUE)$cov))

#    rse <- lapply(.self$zelig.out$z.out,
#        (function(x) {
#            full <- quantreg::summary.rq(x, se = "nid", cov = TRUE)$cov
#        })
#    )
    .self$test.statistics<- list(robust.se = rse)
})

zquantile$methods(
  param = function(z.out, method = "mvn") {
    object <- z.out
    if(identical(method,"mvn")){
        rq.sum <- summary(object, cov = TRUE, se = object$se)
        return(mvrnorm(n = .self$num, mu = object$coef, Sigma = rq.sum$cov))
    } else if(identical(method,"point")){
        return(t(as.matrix(object$coef)))
    }
})

zquantile$methods(
  qi = function(simparam, mm) {
    object <- mm
    coeff <- simparam
    eps <- .Machine$double.eps^(2/3)
    ev <- coeff %*% t(object)
    pv <- ev
    n <- nrow(.self$data)
    h <- bandwidth.rq(.self$tau, n) # estimate optimal bandwidth for sparsity
    if (.self$tau + h > 1)
      stop("tau + h > 1. Sparsity estimate failed. Please specify a tau closer to 0.5")
    if (.self$tau - h < 0)
      stop("tau - h < 0. Sparsity estimate failed. Please specify a tau closer to 0.5")
    beta_high <- rq(.self$formula, data = .self$data, tau = .self$tau + h )$coef
    beta_low <- rq(.self$formula, data = .self$data, tau = .self$tau - h)$coef
    F_diff <- mm %*% (beta_high - beta_low)
    if (any(F_diff <= 0))
      warning(paste(sum(F_diff <= 0),
                    "density estimates were non-positive. Predicted values will likely be non-sensical."))
    # Includes machine error correction as per summary.rq for nid case
    f <- pmax(0, (2 * h) / (F_diff - eps))
    # Use asymptotic approximation of Q(tau|X,beta) distribution
    for(ii in 1:nrow(ev))
      # Asymptotic distribution as per Koenker 2005 _Quantile Regression_ p. 72
      pv[ii, ] <- rnorm(length(ev[ii, ]), mean = ev[ii, ],
                        sqrt((.self$tau * (1 - .self$tau))) / (f * sqrt(n)))
    return(list(ev  = ev, pv = pv))
  }
)
