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
  qi = function(simparam, mm) {
    myorder <- eval(.self$zelig.call$order)
    mycoef <- coef(.self$zelig.out$z.out[[1]])

    acf <- simacf(coef=mycoef, order=myorder, params=simparam, alpha=0.05)
    ev <- 1
    pv <- 1
    return(list(acf = acf, ev = ev, pv = pv))
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



#' Construct Autocorrelation Function from Zelig object and simulated parameters
#' @keywords internal


simacf <- function(coef, order, params, alpha = 0.5){

  #order <- eval(.self$zelig.call$order)
  myar <- myma <- myar.seq <- myma.seq <- NULL

  if(order[1]>0){
    arnames <- paste("ar", 1:order[1], sep="")
    myar <- coef[arnames]
    myar.seq <- params[,arnames]
  }

  if(order[3]>0){
    manames <- paste("ma", 1:order[1], sep="")
    myma <- coef[manames]
    myma.seq <- params[,manames]
  }

  mylag.max<-10  # Need to set automatically

  n.sims<-nrow(params)

  expected.acf <- ARMAacf(ar=myar, ma=myma, lag.max=mylag.max)
  acf.history<-matrix(NA, nrow=n.sims, ncol=length(expected.acf))      # length(expected.acf) = mylag.max +1 
  for(i in 1:n.sims){
    acf.history[i,] <- ARMAacf(ar=myar.seq[i], ma=myma.seq[i], lag.max=mylag.max)
  }


  # Define functions to compute confidence intervals for each column in a matrix
  ci.matrix <- function(x, alpha) {
    pos.hi <- max(round((1-(alpha/2))*nrow(x)), 1)
    pos.low <-max(round((alpha/2)*nrow(x)), 1)

    ci.lower <- ci.upper <- rep(NA, ncol(x))
    for(i in 1:ncol(x)){
        temp<-sort(x[,i])
        ci.lower[i]<-temp[pos.low]
        ci.upper[i]<-temp[pos.hi]
    }
    return(list(ci.lower=ci.lower, ci.upper=ci.upper))
  }

  ci.acf <- ci.matrix(x=acf.history, alpha=0.05)

  return(list(expected.acf=expected.acf, ci.acf=ci.acf, sims.acf=acf.history))
}
