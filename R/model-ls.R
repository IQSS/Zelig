#' Least Squares Regression for Continuous Dependent Variables
#' 
#' @param formula an object of class "formula" (or one that can be coerced to 
#'   that class): a symbolic description of the model to be fitted. 
#' @param data an optional data frame, list or environment (or object coercible 
#'   by `as.data.frame` to a data frame) containing the variables in the model. 
#'   If not found in data, the variables are taken from `environment(formula)`, 
#'   typically the environment from which `lm`` is called. 
#'   
#'   Imputed data sets created with \link{amelia} or \code{\link{to_zelig_mi}} 
#'   can also be supplied.
#' @param ... arguments to pass to \code{\link{lm}}.
#' @param model a character string specifying the model type. NOTE: only 
#'   required if using the Zelig 4 wrappers.
#' @param weights an optional vector of weight values or character string 
#'   identifying a weighting variable in `data`. Weights adjust the observed 
#'   sample distribution in the data to an underlying population of interest. 
#'   
#'   - If the supplied weights are all integer values, then `zelig` rebuilds a new 
#'   version of the dataset by duplicating observations according to their 
#'   weight (and removing observations with zero weight).
#'   
#'   - If the weights are continuously valued, `zelig`` bootstraps the supplied 
#'   dataset, using the relative weights as bootstrap probabilities.
#'   
#' @param by an optional character string identifying a variable in `data` 
#'   to conduct the model analysis along. This can be used to run separate 
#'   models for different levels of a categorical variable.
#' @param bootstrap logical whether or not to bootstrap the data set.
#' @param cite logical whether or not to return model citation information to 
#'   the console when the model has been estimated. 
#'
#' @details Use least squares regression analysis to estimate the best linear 
#'   predictor for the specified dependent variables.
#' 
#' Vignette: [http://docs.zeligproject.org/en/latest/zelig-ls.html](http://docs.zeligproject.org/en/latest/zelig-ls.html)
#' 
#' @examples
#' \dontrun{
#' # Workflow: Zelig 5 Reference Classes
#' z5 <- zls$new()
#' z5$zelig(Y ~ X1 + X ~ X, weights = w, data = mydata)
#' z5$setx()
#' z5$sim()
#' z5$graph()
#' 
#' # Workflow: Zelig 4 Wrappers
#' z.out <- zelig(Y ~ X1 + X2, model = "ls", weights = w, data = mydata)
#' x.out <- setx(z.out)
#' s.out <- sim(z.out, x = x.out)
#' plot(s.out)
#' }
#' 
#' 
#' # Basic Example with First Differences (using Zelig 4 wrappers) #
#' 
#' data(macro)
#' 
#' # Estimate model
#' z.out1 <- zelig(unem ~ gdp + capmob + trade, model = "ls", data = macro)
#' 
#' # Set explanatory variables to their default (mean/mode) values, with high 
#' # (80th percentile) and low (20th percentile) values for the trade variable
#' x.high <- setx(z.out1, trade = quantile(macro$trade, 0.8))
#' x.low <- setx(z.out1, trade = quantile(macro$trade, 0.2))
#' 
#' # Simulate quantities of interest
#' s.out1 <- sim(z.out1, x = x.high, x1 = x.low)
#' 
#' \dontrun{
#' # Plot first differences
#' plot(s.out1)
#' }
#' 
#' @import methods
#' @exportClass Zelig-ls
#'
#' @include model-zelig.R
#' @md

zls <- setRefClass("Zelig-ls", contains = "Zelig")

zls$methods(
  initialize = function() {
    callSuper()
    .self$name <- "ls"
    .self$year <- 2007
    .self$category <- "continuous"
    .self$description <- "Least Squares Regression for Continuous Dependent Variables"
    .self$packageauthors <- "R Core Team"
    .self$fn <- quote(stats::lm)
    # JSON
    .self$outcome <- "continous"
    .self$wrapper <- "ls"
    .self$acceptweights <- TRUE
  }
)

zls$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL, bootstrap = FALSE) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    callSuper(formula = formula, data = data, ...,
              weights = weights, by = by, bootstrap = bootstrap)
    
    # Automated Background Test Statistics and Criteria
    rse <- lapply(.self$zelig.out$z.out, (function(x) vcovHC(x, type = "HC0")))
    rse.se <- sqrt(diag(rse[[1]]))                 # Needs to work with "by" argument
    est.se <- sqrt(diag(.self$get_vcov()[[1]]))
    quickGim <- any( est.se > 1.5*rse.se | rse.se > 1.5*est.se )
    .self$test.statistics<- list(robust.se = rse, gim.criteria = quickGim)
  }
)

zls$methods(
  param = function(z.out, method="mvn") {
    if(identical(method,"mvn")){
      return(list(simparam=mvrnorm(.self$num, coef(z.out), vcov(z.out)), simalpha=rep( summary(z.out)$sigma, .self$num) )  )
    } else if(identical(method,"point")){
      return(list(simparam=t(as.matrix(coef(z.out))), simalpha=summary(z.out)$sigma))
    } else {
      stop("param called with method argument of undefined type.")
    }
  }
)

zls$methods(
  qi = function(simparam, mm) {
    ev <- simparam$simparam %*% t(mm)
    pv <- as.matrix(rnorm(n=length(ev), mean=ev, sd=simparam$simalpha), nrow=length(ev), ncol=1)
    return(list(ev = ev, pv = pv))
  }
)

zls$methods(
  gim = function(B=50, B2=50) {
    ll.normal.bsIM <- function(par,y,X,sigma){
        beta <- par[1:length(X)]
        sigma2 <- sigma
        -1/2 * (sum(log(sigma2) + (y -(X%*%beta))^2/sigma2))
    }
    
    getVb<-function(Dboot){
      Dbar <- matrix(apply(Dboot,2,mean),nrow=B, ncol=length(Dhat), byrow=TRUE)
      Diff <- Dboot - Dbar
      Vb <- (t(Diff) %*% Diff) / (nrow(Dboot)-1)
      return(Vb)
    }
    
    getSigma<-function(lm.obj){
      return(sum(lm.obj$residuals^2)/(nrow(model.matrix(lm.obj))-ncol(model.matrix(lm.obj))))
    }
    
    D.est<-function(formula,data){
      lm1 <- lm(formula,data, y=TRUE)
      mm <- model.matrix(lm1)
      y <- lm1$y
      sigma <- getSigma(lm1)
    
      grad <- apply(cbind(y,mm),1,function(x) numericGradient(ll.normal.bsIM, lm1$coefficients, y=x[1], X=x[2:length(x)], sigma=sigma))
      meat <- grad%*%t(grad)
      bread <- -solve(vcov(lm1))
      Dhat <- nrow(mm)^(-1/2)* as.vector(diag(meat + bread))
      return(Dhat)
    }

    D.est.vb<-function(formula,data){
        lm1 <- lm(formula,data, y=TRUE)
        mm <- model.matrix(lm1)
        y <- lm1$y
        sigma <- getSigma(lm1)
        
        grad <- apply(cbind(y,mm),1,function(x) numericGradient(ll.normal.bsIM, lm1$coefficients, y=x[1], X=x[2:length(x)], sigma=sigma))
        meat <- grad%*%t(grad)
        bread <- -solve(vcov(lm1))
        Dhat <- nrow(mm)^(-1/2)* as.vector(diag(meat + bread))

        muB<-lm1$fitted.values
        DB <- matrix(NA, nrow=B2, ncol=length(Dhat))
            
        for(j in 1:B2){
          yB2 <- rnorm(nrow(data), muB, sqrt(sigma))
          lm1B2 <- lm(yB2 ~ mm-1)
          sigmaB2 <- getSigma(lm1B2)

          grad <- apply(cbind(yB2,model.matrix(lm1B2)),1,function(x) numericGradient(ll.normal.bsIM, lm1B2$coefficients, y=x[1], X=x[2:length(x)], sigma=sigmaB2))
          meat <- grad%*%t(grad)
          bread <- -solve(vcov(lm1B2))
          DB[j,] <- nrow(mm)^(-1/2)*diag((meat + bread))
        }
        Vb <- getVb(DB)
        T<- t(Dhat)%*%solve(Vb)%*%Dhat

        return(list(Dhat=Dhat,T=T))
    }
    
    Dhat <- D.est(formula=.self$formula, data=.self$data)
    lm1 <- lm(formula=.self$formula, data=.self$data)
    mu <- lm1$fitted.values
    sigma <- getSigma(lm1)
    n <- length(mu)
    yname <- all.vars(.self$formula[[2]])
    
    Dboot <- matrix(NA, nrow=B, ncol=length(Dhat))
    bootdata<-data
    for(i in 1:B){
        yB <- rnorm(n, mu, sqrt(sigma))
        bootdata[yname] <- yB
        result <- D.est.vb(formula=.self$formula, data=bootdata)
        Dboot[i,] <- result$Dhat
        T[i] <- result$T
    }

    Vb <- getVb(Dboot)
    omega <- t(Dhat) %*% solve(Vb) %*% Dhat
    pb = (B+1-sum(T< as.numeric(omega)))/(B+1)
    
    .self$test.statistics$gim <- list(stat=omega, pval=pb)

    # When method used, add to references
    gimreference <- bibentry(
        bibtype="Article",
        title = "How Robust Standard Errors Expose Methodological Problems They Do Not Fix, and What to Do About It",
        author = c(
        person("Gary", "King"),
        person("Margret E.", "Roberts")
        ),
        journal = "Political Analysis",
        year = 2014,
        pages = "1-21",
        url =  "http://j.mp/InK5jU")
    .self$refs <- c(.self$refs, gimreference)
  }
)

zls$methods(
  mcfun = function(x, b0=0, b1=1, alpha=1, sim=TRUE){
    y <- b0 + b1*x + sim * rnorm(n=length(x), sd=alpha)
    return(y)
  }
)
