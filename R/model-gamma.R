#' Gamma Regression for Continuous, Positive Dependent Variables
#'
#' Vignette: \url{http://docs.zeligproject.org/articles/zelig_gamma.html}
#' @import methods
#' @export Zelig-gamma
#' @exportClass Zelig-gamma
#' 
#' @include model-zelig.R
#' @include model-glm.R

zgamma <- setRefClass("Zelig-gamma",
                      contains = "Zelig-glm")

zgamma$methods(
  initialize = function() {
    callSuper()
    .self$name <- "gamma"
    .self$family <- "Gamma"
    .self$link <- "inverse"
    .self$authors <- "Kosuke Imai, Gary King, Olivia Lau"
    .self$year <- 2007
    .self$category <- "bounded"
    .self$description <- "Gamma Regression for Continuous, Positive Dependent Variables"
    # JSON
    .self$outcome <- "continous"
    .self$wrapper <- "gamma"
  }
)

zgamma$methods(
  param = function(z.out, method="mvn") {
    shape <- MASS::gamma.shape(z.out)
    if(identical(method, "mvn")){
      simalpha <- rnorm(n = .self$num, mean = shape$alpha, sd = shape$SE)
      simparam.local <- mvrnorm(n = .self$num, mu = coef(z.out), Sigma = vcov(z.out))
      simparam.local <- list(simparam = simparam.local, simalpha = simalpha)
      return(simparam.local)
    } else if(identical(method,"point")){
      return(list(simparam = t(as.matrix(coef(z.out))), simalpha = shape$alpha ))
    }
  }
)

zgamma$methods(
  qi = function(simparam, mm) {
    coeff <- simparam$simparam 
    eta <- (coeff %*% t(mm) ) * simparam$simalpha  # JH need to better understand this parameterization.  Coefs appear parameterized so E(y_i) = 1/ (x_i\hat{\beta})
    theta <- matrix(1 / eta, nrow = nrow(coeff), ncol=1)
    ev <- theta * simparam$simalpha 
    pv<- matrix(rgamma(nrow(ev), shape = simparam$simalpha, scale = theta), nrow=nrow(ev), ncol=1)
    return(list(ev = ev, pv = pv))
  }
)

zgamma$methods(
  mcfun = function(x, b0=0, b1=1, alpha=1, sim=TRUE){
    lambda <- 1/(b0 + b1 * x)
    if(sim){
        y <- rgamma(n=length(x), shape=alpha, scale = lambda)
        return(y)
    }else{
        return(alpha * lambda)
    }
  }
)

