#' Linear Regression for a Left-Censored Dependent Variable
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
#' @param below: point at which the dependent variable is censored from below.
#'     If the dependent variable is only censored from above, set \code{below = -Inf}.
#'     The default value is 0.
#' @param above: point at which the dependent variable is censored from above.
#'      If the dependent variable is only censored from below, set \code{above = Inf}.
#'      The default value is \code{Inf}.
#'
#' @examples
#' data(turnout)
#' z.out <- zelig(vote ~ race + educate, model = "logit.bayes",data = turnout, verbose = FALSE)
#'
#' @details
#'
#'
#'
#' Additional parameters avaialable to many models include:
#' \itemize{
#'   \item weights: vector of weight values or a name of a variable in the dataset
#'   by which to weight the model. For more information see:
#'   \url{http://docs.zeligproject.org/articles/weights.html}.
#'   \item burnin: number of the initial MCMC iterations to be discarded (defaults to 1,000).
#'   \item mcmc: number of the MCMC iterations after burnin (defaults to 10,000).
#'   \item thin: thinning interval for the Markov chain. Only every thin-th draw from the Markov chain is kept. The value of mcmc must be divisible by this value. The default value is 1.
#'   \item verbose: defaults to FALSE. If TRUE, the progress of the sampler (every 10\%) is printed to the screen.
#'   \item seed: seed for the random number generator. The default is \code{NA} which corresponds to a random seed of 12345.
#'   \item beta.start: starting values for the Markov chain, either a scalar or vector with length equal to the number of estimated coefficients. The default is \code{NA}, such that the maximum likelihood estimates are used as the starting values.
#' }
#' Use the following parameters to specify the model’s priors:
#' \itemize{
#'     \item b0: prior mean for the coefficients, either a numeric vector or a scalar. If a scalar value, that value will be the prior mean for all the coefficients. The default is 0.
#'     \item B0: prior precision parameter for the coefficients, either a square matrix (with the dimensions equal to the number of the coefficients) or a scalar. If a scalar value, that value times an identity matrix will be the prior precision parameter. The default is 0, which leads to an improper prior.
#'     \item c0: c0/2 is the shape parameter for the Inverse Gamma prior on the variance of the disturbance terms.
#'     \item d0: d0/2 is the scale parameter for the Inverse Gamma prior on the variance of the disturbance terms.
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
#' Vignette: \url{http://docs.zeligproject.org/articles/zelig_tobit.html}
#' @import methods
#' @export Zelig-tobit
#' @exportClass Zelig-tobit
#'
#' @include model-zelig.R

ztobit <- setRefClass("Zelig-tobit",
                      contains = "Zelig",
                      fields = list(above = "numeric",
                                    below = "numeric"))

ztobit$methods(
  initialize = function() {
    callSuper()
    .self$name <- "tobit"
    .self$authors <- "Kosuke Imai, Gary King, Olivia Lau"
    .self$packageauthors <- "Christian Kleiber, and Achim Zeileis"
    .self$year <- 2011
    .self$description = "Linear regression for Left-Censored Dependent Variable"
    .self$fn <- quote(AER::tobit)
    # JSON
    .self$outcome <- "continous"
    .self$wrapper <- "tobit"
    .self$acceptweights <- TRUE
  }
)

ztobit$methods(
  zelig = function(formula, ..., below = 0, above = Inf,
                   robust = FALSE, data, weights = NULL, by = NULL, bootstrap = FALSE) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    .self$below <- below
    .self$above <- above
    .self$model.call$below <- NULL
    .self$model.call$above <- NULL
    .self$model.call$left <- below
    .self$model.call$right <- above
    callSuper(formula = formula, data = data, ..., weights = weights, by = by, bootstrap = bootstrap)

    if(!robust){
        fn2 <- function(fc, data) {
            fc$data <- data
            return(fc)
        }
        robust.model.call <- .self$model.call
        robust.model.call$robust <- TRUE

        robust.zelig.out <- .self$data %>%
        group_by_(.self$by) %>%
        do(z.out = eval(fn2(robust.model.call, quote(as.data.frame(.))))$var )

        .self$test.statistics<- list(robust.se = robust.zelig.out$z.out)
    }
  }
)


ztobit$methods(
  param = function(z.out, method="mvn") {
    if(identical(method,"mvn")){
      mu <- c(coef(z.out), log(z.out$scale))
      simfull <- mvrnorm(n = .self$num, mu = mu, Sigma = vcov(z.out))
      simparam.local <- as.matrix(simfull[, -ncol(simfull)])
      simalpha <- exp(as.matrix(simfull[, ncol(simfull)]))
      simparam.local <- list(simparam = simparam.local, simalpha = simalpha)
      return(simparam.local)
    } else if(identical(method,"point")){
      return(list(simparam = t(as.matrix(coef(z.out))), simalpha = log(z.out$scale) ))
    }
  }
)

ztobit$methods(
  qi = function(simparam, mm) {
    Coeff <- simparam$simparam %*% t(mm)
    SD <- simparam$simalpha
    alpha <- simparam$simalpha
    lambda <- dnorm(Coeff / SD) / (pnorm(Coeff / SD))
    ev <- pnorm(Coeff / SD) * (Coeff + SD * lambda)
    pv <- ev
    pv <- matrix(nrow = nrow(ev), ncol = ncol(ev))
    for (j in 1:ncol(ev)) {
      pv[, j] <- rnorm(nrow(ev), mean = ev[, j], sd = SD)
      pv[, j] <- pmin(pmax(pv[, j], .self$below), .self$above)
    }
    return(list(ev = ev, pv = pv))
  }
)

ztobit$methods(
  mcfun = function(x, b0=0, b1=1, alpha=1, sim=TRUE){
    mu <- b0 + b1 * x
    ystar <- rnorm(n=length(x), mean=mu, sd=alpha)
    if(sim){
        y <- (ystar>0) * ystar  # censoring from below at zero
        return(y)
    }else{
        y.uncensored.hat.tobit<- mu + dnorm(mu, mean=0, sd=alpha)/pnorm(mu, mean=0, sd=alpha)
        y.hat.tobit<- y.uncensored.hat.tobit * (1- pnorm(0, mean=mu, sd=alpha) )  # expected value of censored outcome
        return(y.hat.tobit)
    }
  }
)
