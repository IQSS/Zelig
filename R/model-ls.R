#' @include model-zelig.R
zls <- setRefClass("Zelig-ls", contains = "Zelig")

zls$methods(
  initialize = function() {
    callSuper()
    .self$name <- "ls"
    .self$year <- 2007
    .self$category <- "continuous"
    .self$description <- "Least Squares Regression for Continuous Dependent Variables"
    .self$fn <- quote(stats::lm)
    # JSON
    .self$outcome <- "continous"
    .self$wrapper <- "ls"
  }
)

zls$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    callSuper(formula = formula, data = data, ...,
              weights = NULL, by = by)
    rse<-llply(.self$zelig.out$z.out, (function(x) vcovHC(x,type="HC0")))
    .self$test.statistics<- list(robust.se = rse)
  }
)

zls$methods(
  param = function(z.out) {
      return(mvrnorm(.self$num, coef(z.out), vcov(z.out)))
  }
)

zls$methods(
  qi = function(simparam, mm) {
    ev <- simparam %*% t(mm)
    pv <- ev
    return(list(ev = ev, pv = pv))
  }
)

zls$methods(
  gim = function(B=50) {
    ll.normal.bsIM <- function(par,y,X,sigma){
        beta <- par[1:length(X)]
        sigma2 <- sigma
        -1/2 * (sum(log(sigma2) + (y -(X%*%beta))^2/sigma2))
    }
    
    D.est<-function(formula,data){
        lm1 <- lm(formula,data, y=TRUE)
        mm <- model.matrix(lm1)
        y <- lm1$y
        sigma <- sum(lm1$residuals^2)/(nrow(model.matrix(lm1))-ncol(model.matrix(lm1)))
        
        grad <- apply(cbind(y,mm),1,function(x) numericGradient(ll.normal.bsIM, lm1$coefficients, y=x[1], X=x[2:length(x)], sigma=sigma))
        meat <- grad%*%t(grad)
        bread <- -solve(vcov(lm1))
        Dhat <- nrow(mm)^(-1/2)* as.vector(diag(meat + bread))
        return(Dhat)
    }
    
    Dhat <- D.est(formula=.self$formula, data=.self$data)
    lm1 <- lm(formula=.self$formula, data=.self$data)
    mu <- lm1$fitted.values
    sigma <- sum(lm1$residuals^2)/(nrow(model.matrix(lm1))-ncol(model.matrix(lm1)))
    n <- length(mu)
    yname <- all.vars(.self$formula[[2]])
    
    Dboot <- matrix(NA, nrow=B, ncol=length(Dhat))
    bootdata<-data
    for(i in 1:B){
        yB <- rnorm(n, mu, sqrt(sigma))
        bootdata[yname] <- yB
        Dboot[i,] <- D.est(formula=.self$formula, data=bootdata)
    }
    
    Dbar <- matrix(apply(Dboot,2,mean),nrow=B, ncol=length(Dhat), byrow=TRUE)
    Diff <- Dboot - Dbar
    Vb <- (t(Diff) %*% Diff) / (B-1)
    omega <- t(Dhat) %*% solve(Vb) %*% Dhat
    
    .self$test.statistics$gim <- list(stat=omega) #, pval=pb)
  }
)



zls$methods(
  test = function(b0 = -1, b1 = 1, nsim = 1000, minx = -1, maxx = 1) {
    x.init <- mcunit.init(nsim, minx, maxx)
    
    b0 <- b0
    b1 <- b1 
    sd <- 1
    
    mu.sim <- b1 * x.init[,1] + b0
    y.sim <- rnorm(nsim, mean = mu.sim, sd = sd)
    y.true <- b1 * x.init[,2] + b0
    data <- cbind(x.init, y.sim, y.true)
    
    z <- zls$new()
    callSuper(z, data)
  }
)

