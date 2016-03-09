#' Gamma Regression with Survey Weights
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-gammasurvey.html}
#' @import methods
#' @export Zelig-gamma
#' @exportClass Zelig-gamma
#' 
#' @include model-zelig.R
#' @include model-survey.R
#' @include model-gamma.R

zgammasurvey <- setRefClass("Zelig-gamma-survey",
                           contains = c("Zelig-survey", "Zelig-gamma"))

zgammasurvey$methods(
  initialize = function() {
    callSuper()
    .self$name <- "gamma-survey"
    .self$family <- "Gamma"
    .self$link <- "inverse"
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$category <- "continuous"
    .self$description = "Gamma Regression with Survey Weights"
    # JSON from parent
    .self$wrapper <- "gamma.survey"
  }
)

zgammasurvey$methods(
  param = function(z.out, method="mvn") {
    shape <- MASS::gamma.shape(z.out)
    if(identical(method,"mvn")){
      simalpha <- rnorm(n = .self$num, mean = shape$alpha, sd = shape$SE)
      simparam.local <- mvrnorm(n = .self$num, mu = coef(z.out),
                                   Sigma = vcov(z.out))
      simparam.local <- list(simparam = simparam.local, simalpha = simalpha)
      return(simparam.local)
    } else if(identical(method,"point")){
      return(list(simparam = t(as.matrix(coef(z.out))), simalpha = shape$alpha))
    }
  }
)

zgammasurvey$methods(
  mcfun = function(x, b0=0, b1=1, alpha=1, sim=TRUE){
    lambda <- 1/(b0 + b1 * x)
    if(sim){
        y <- rgamma(n=length(x), shape=lambda, scale = alpha)
        return(y)
    }else{
        return(alpha * lambda)
    }
  }
)