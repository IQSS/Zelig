#' Autoregressive and Moving-Average Models for Time-Series Data
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-arma.html}
#' @import methods
#' @export Zelig-arma
#' @exportClass Zelig-arma
#'
#' @include model-zelig.R
#' @include model-timeseries.R

zarma <- setRefClass("Zelig-arma",
                        contains = "Zelig-timeseries")

zarma$methods(
  initialize = function() {
    callSuper()
    .self$name <- "arma"
    .self$link <- "identity"
    #.self$family <- "gaussian"
    .self$fn <- quote(zeligArimaWrapper)
    #.self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$category <- "continuous"
    .self$description <- "Autoregressive Moving-Average Models for Time-Series Data"
    # JSON
    .self$outcome <- "continuous"
    .self$wrapper <- "timeseries"
  }
)

zarma$methods(
  qi = function(simparam) {
    acf <- simacf(simparam)
    ev <- 1
    pv <- 1
    return(list(acf <- acf, ev = ev, pv = pv))
  }
)

zarma$methods(
  mcfun = function(x, b0=0, b1=1, ..., sim=TRUE){
    mu <- exp(b0 + b1 * x)
    if(sim){
      y <- rnorm(n=length(x), mean=mu)
      return(y)
    }else{
      return(mu)
    }
  }
)

#' Estimation wrapper function for arima models, to easily fit with Zelig architecture
#' @keywords internal

zeligArimaWrapper <- function(formula, order=c(1,0,0), ... , include.mean=TRUE, data){
    
    # Using with():
    # myArimaCall <- quote( arima(x=, order =, xreg= ) )
    # output <- with(data, myArimaCall )


    # Using arima() directly:
    mf <- model.frame(formula, data)

    acf3 <- as.character(formula[[3]])
    
    yflag <- names(mf) %in% all.vars(formula[-3]) 
    xflag <- names(mf) %in% all.vars(formula[-2]) 
    
    myx <- as.matrix(mf[,yflag, drop=FALSE])  # could use get_all_vars()
    myxreg <- as.matrix(mf[,xflag, drop=FALSE])
    
    if (("1" %in% acf3 ) & ("-" %in% acf3 )){
        include.mean <- FALSE
    }
    
    output <- stats::arima(x=myx, order=order, xreg=myxreg, include.mean=include.mean, ...)

}

