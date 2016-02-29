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
    .self$description <- "Autoregressive Moving-Average Models for Time-Series Data"
    # JSON
    .self$outcome <- "continuous"
    .self$wrapper <- "timeseries"
  }
)

zarma$methods(
  qi = function(simparam, mm, mm1=NULL){ 
    myorder <- eval(.self$zelig.call$order)
    mycoef <- coef(.self$zelig.out$z.out[[1]])
    sd <- sqrt(.self$zelig.out$z.out[[1]]$sigma2)


    acf <- simacf(coef=mycoef, order=myorder, params=simparam, alpha=0.05)
    acf.length <- length(acf$expected.acf)

    t1 <- 2*acf.length
    t2 <- 2*acf.length


    if(.self$bsetx1){             # could also check if mm1 is NULL
      # zeligARMAbreakforecaster() calls zeligARMAlongrun() internally
      yseries <- zeligARMAbreakforecaster(y.init=NULL, x=mm, x1=mm1, simparam=simparam, order=myorder, sd=sd, t1=t1, t2=t2) 

      # maybe check nrow(yseries)=t1 + t2 ?

      pv <- yseries$innovation[t1,]                # could use either $innovation or $shock here
      pv.shortrun <- yseries$innovation[t1+1,]     # could use either $innovation or $shock here
      pv.longrun <- yseries$innovation[t1+t2,]     # must use $innovation here

      # Remember, these are expectations using the same simparam in each expectation.
      ev <- rnorm(n=nrow(simparam))
      ev.shortrun <- rnorm(n=nrow(simparam))
      ev.longrun <- rnorm(n=nrow(simparam))
      return(list(acf = acf, ev = ev, pv = pv, pv.shortrun=pv.shortrun, pv.longrun=pv.longrun, ev.shortrun=ev.shortrun, ev.longrun=ev.longrun, 
                range.shock=yseries.shock, range.innovation=yseries.innovation))

    }else{
      # just call zeligARMAlongrun()
      yseries <- zeligARMAlongrun(y.init=NULL, x=mm, simparam=simparam, order=myorder, sd=sd) 
 #     print("got here A")
      pv <- yseries$y[1,]   # zeligARMAlongrun returns the series in reverse order to zeligARMAbreakforecaster
#      print(yseries$y[,1:5])
#      print(dim(pv))
#      print("got here B")
#      stop()

      # Remember, these are expectations using the same simparam in each expectation.
      ev <- rnorm(n=nrow(simparam))
      return(list(acf = acf, ev = ev, pv = pv))
    }
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
    myar.seq <- params[, arnames, drop=FALSE]
  }

  if(order[3]>0){
    manames <- paste("ma", 1:order[3], sep="")
    myma <- coef[manames]
    myma.seq <- params[, manames, drop=FALSE]
  }

  mylag.max<-10  # Need to set automatically.  

  n.sims<-nrow(params)

  expected.acf <- ARMAacf(ar=myar, ma=myma, lag.max=mylag.max)
  acf.history<-matrix(NA, nrow=n.sims, ncol=length(expected.acf))      # length(expected.acf) = mylag.max +1 
  for(i in 1:n.sims){
    acf.history[i,] <- ARMAacf(ar=myar.seq[i,], ma=myma.seq[i,], lag.max=mylag.max)
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


#' Construct Simulated Next Step in Dynamic Series
#' @keywords internal

zeligARMAnextstep <- function(yseries=NULL, xseries, wseries=NULL, beta, ar=NULL, i=NULL, ma=NULL, sd){
  
  ## Check inputs
  # t is obs across time
  # s is sims
  # k is covariates
  # order is (p,q,r)
  # assume yseries (t x sims), xseries (t x k), wseries (t x s), beta (s x k), ar (s x p), ma (s x r) are matrix
  # assume sd is scalar

  ## Could construct these by using known order more deliberatively

  if(is.vector(yseries)){
    #print("warning: yseries is vector")
    yseries <- matrix(yseries, nrow=1)        # Assume if y is a vector, that we are only running one simulation chain of y, so y is (t x 1)
  }
  if(is.vector(xseries)){
    #print("warning: xseries is vector")
    xseries <- matrix(xseries, nrow=1)        # Assume if x is a vector, that there are no lagged terms, so x is (1 x k)
  }
  if(is.vector(wseries)){
    #print("warning: wseries is vector")
    wseries <- matrix(wseries, nrow=1)        # Assume if w is a vector, that we are only running one simulation chain of y, so w is (t x 1)
  }
  if(is.vector(beta)){
    #print("warning: beta is vector")
    beta <- matrix(beta, ncol=1)
  }
  if(is.vector(ar)){
    #print("warning: ar is vector")
    ar <- matrix(ar, ncol=1)
  }
  if(is.vector(ma)){
    #print("warning: ma is vector")
    ma <- matrix(ma, ncol=1)
  }

#print(ar)
#print(yseries)
#stop()

  ar.term <- function(yseries, ar, n){
    yshort <- yseries[1:ncol(ar), , drop=FALSE]           # because we only need the diagonal of a square matrix, we can avoid full matrix multiplication
    return( rowSums( ar * t(yshort) ) )       # diag[(s x p) . (p x s)] = diag[(s x s)] = (s x 1)  
  }
  xt.term <- function(xseries, beta){
    return( as.vector(beta %*% t(xseries)) )  # (s x k) . t(1 x k) = (s x 1)
  }
  ma.term <- function(wseries, ma){    
    wshort <- wseries[1:ncol(ma), , drop=FALSE]
    return( rowSums( ma * t(wshort)) )        # diag[(s x r) . (r x s)] = diag[(s x s)] = (s x 1)
  }

  n.sims <- ncol(yseries)   
  w <- rnorm(n=n.sims, mean=0, sd=sd)
  y <- xt.term(xseries,beta) + w              # conformable if xt is vector and w vector
  if(!is.null(ar)){
    y <- y + ar.term(yseries,ar)              # conformable if y vector and ar vector 
  }
  if(!is.null(ma)){
    y <- y + ma.term(wseries,ma)              # conformable if y vector and ma vector 
  }

  return(list(y=y, w=w))
}


#' Calculate the Long Run Exquilibrium for Fixed X
#' @keywords internal

zeligARMAlongrun <- function(y.init=NULL, x, simparam, order, sd, tol=NULL, burnin=20){
  if(is.null(tol)){
    tol<-0.01
  }

  ar <- i <- ma <- NULL

  xnames <- colnames(x)

  #beta.test <- names(simparam) %in% xnames 
  #if(sum(beta.test) < ncol(x)){
  #  print("warning: provided covariates and simulated parameters do not seem to match.")
  #}

  ### -- revise this approach:  ###
  if("(Intercept)" %in% xnames){
    flag <- xnames == "(Intercept)"
    xnames[flag] <- "intercept"
  }

  beta <- simparam[,xnames]

  if(order[1]>0){
    arnames <- paste("ar", 1:order[1], sep="")
    ar <- simparam[,arnames]
  }

  if(order[3]>0){
    manames <- paste("ma", 1:order[3], sep="")
    ma <- simparam[,manames]
  }

  timepast <- max(order[1],order[3])
  n.sims <- nrow(simparam)

  if(is.vector(x)){
    x<-matrix(x,nrow=1, ncol=length(x))
  }

  if(is.null(y.init)){
    betabar <- t(apply(beta,2, mean))
    y.init <- x %*% t(beta)
  }

  yseries <- matrix(y.init, nrow=timepast, ncol=n.sims, byrow=TRUE)
  wseries <- matrix(rnorm(n=timepast*n.sims), nrow=timepast, ncol=n.sims)

  finished <- FALSE
  count <- 0
  while(!finished){
    #cat("*************************** \n")
    #cat(paste("Count is ", count, "\n"))
    #cat(paste("yseries is: \n"))
    #print(yseries[,1:5])
    y <- zeligARMAnextstep(y=yseries[1:timepast, ], x=x, wseries=wseries[1:timepast, ], beta=beta, ar=ar, i=i, ma=ma, sd=sd)
    #print(y$y[1:10])
    #plot(density(y$y))
    #abline(v=mean(y.init))
    yseries <- rbind(y$y, yseries)
    wseries <- rbind(y$w, wseries)
    #print(yseries[,1:5])
    #cat("-=-=-=-=-=-=-=-=-=-=-=-=-=- \n")


    #diff <- mean(abs(y.1 - y.0))  # Eventually need to determine some automated stopping rule
    count <- count+1
    finished <- count>burnin #| (diff < tol)
  }

  return(list(y.longrun=yseries, w.longrun=wseries))
}


#' Construct Simulated Series with Internal Discontinuity in X
#' @keywords internal

zeligARMAbreakforecaster <- function(y.init=NULL, x, x1, simparam, order, sd, t1=5, t2=10){

  longrun.out <- zeligARMAlongrun(y.init=y.init, x=x, simparam=simparam, order=order, sd=sd)   

  yseries <- longrun.out$y.longrun
  wseries <- longrun.out$wseries

  xnames <- names(x)
  beta.test <- names(simparam) %in% xnames 
  if(sum(beta.test) < ncol(x)){
    print("warning: provided covariates and simulated parameters do not seem to match.")
  }
  beta <- simparam[,xnames]

  ar <- i <- ma <- NULL
  if(order[1]>0){                                      # Should we track this value and use below?
    arnames <- paste("ar", 1:order[1], sep="")
    ar <- simparam[,arnames]
  }

  if(order[3]>0){
    manames <- paste("ma", 1:order[3], sep="")
    ma <- simparam[,manames]
  }

  timepast <- max(order[1],order[3]) # How many steps backward are needed in the series  --  could we be more precise?

  # Take a step at covariates x
  for(i in 2:t1){
    nextstep <- zeligARMAnextstep(y=yseries[1:timepast, ], x=x, wseries=wseries[1:timepast, ], beta=beta, ar=ar, i=i, ma=ma, sd=sd)
    yseries <- rbind(nextstep$y, yseries)   # Could just change arguments so nextstep(nextstep) doesn't need to copy elsewhere.
    wseries <- rbind(nextstep$w, wseries)
  }

  # Introduce shock
    nextstep <- zeligARMAnextstep(y=yseries[1:timepast, ], x=x1, wseries=wseries[1:timepast, ], beta=beta, ar=ar, i=i, ma=ma, sd=sd)
    yseries <- rbind(nextstep$y, yseries)   # Could just change arguments so nextstep(nextstep) doesn't need to copy elsewhere.
    wseries <- rbind(nextstep$w, wseries)
    y.innov <- yseries
    w.innov <- wseries  # Note: sequence of stocastic terms are going to depart now

  for(i in 2:t2){
    # Take further steps at covariates x1 (an introduction of an innovation)
    nextstep <- zeligARMAnextstep(y=y.innov[1:timepast, ], x=x1, wseries=w.innov[1:timepast, ], beta=beta, ar=ar, i=i, ma=ma, sd=sd)
    y.innov <- rbind(nextstep$y, y.innov)  # Could just change arguments so nextstep(nextstep) doesn't need to copy elsewhere.
    w.innov <- rbind(nextstep$w, w.innov)
    # And take steps returning to old covariates (an introduction of a shock)
    nextstep <- zeligARMAnextstep(y=yseries[1:timepast, ], x=x, wseries=wseries[1:timepast, ], beta=beta, ar=ar, i=i, ma=ma, sd=sd)
    yseries <- rbind(nextstep$y, yseries)   # Could just change arguments so nextstep(nextstep) doesn't need to copy elsewhere.
    wseries <- rbind(nextstep$w, wseries)
  }

  yseries <- yseries[t1 + t2, ]  # Truncate series to last periods, removing burn-in to equilibrium
  y.innov <- y.innov[t1 + t2, ]

  yseries <- yseries[nrow(yseries):1,]  # Change y to conventional row ordering by time before returning
  y.innov <- y.innov[nrow(y.innov):1,]

  return(y.shock = yseries, y.innovation = y.innov)  
}
