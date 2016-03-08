#' Normal Regression for Continuous Dependent Variables with Survey Weights
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-normalsurvey.html}
#' @import methods
#' @export Zelig-normal
#' @exportClass Zelig-normal
#'
#' @include model-zelig.R
#' @include model-survey.R
#' @include model-normal.R


znormalsurvey <- setRefClass("Zelig-normal-survey",
                       contains = c("Zelig-survey"),
                       fields = list(family = "character",
                                  link = "character",
                                  linkinv = "function"))
                                  #, "Zelig-normal"))

znormalsurvey$methods(
  initialize = function() {
    callSuper()
    .self$name <- "normal-survey"
    .self$family <- "gaussian"
    .self$link <- "identity"
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$category <- "continuous"
    .self$description <- "Normal Regression for Continuous Dependent Variables with Survey Weights"
    .self$outcome <- "continuous"
    # JSON
    .self$wrapper <- "normal.survey"
  }
)

znormalsurvey$methods(
  param = function(z.out, method="mvn") {
    degrees.freedom <- z.out$df.residual
    sig2 <- base::summary(z.out)$dispersion # not to call class summary method
    simalpha <- sqrt(degrees.freedom * sig2 
                     / rchisq(.self$num, degrees.freedom))

    if(identical(method,"mvn")){
      simparam.local <- mvrnorm(n = .self$num,
                              mu = coef(z.out),
                              Sigma = vcov(z.out))
      simparam.local <- list(simparam = simparam.local, simalpha = simalpha)
      return(simparam.local)
    } else if(identical(method,"point")){
      return(list(simparam = t(as.matrix(coef(z.out))), simalpha = simalpha))
    }

  }
)

znormalsurvey$methods(
  qi = function(simparam, mm) {
    theta <- matrix(simparam$simparam %*% t(mm),
                    nrow = nrow(simparam$simparam))
    ev <- theta
    pv <- matrix(NA, nrow = nrow(theta), ncol = ncol(theta))
    for (j in 1:nrow(ev))
      pv[j, ] <- rnorm(ncol(ev),
                       mean = ev[j, ],
                       sd = simparam$simalpha[j])
    return(list(ev = ev, pv = pv))
  }
)

znormalsurvey$methods(
  mcfun = function(x, b0=0, b1=1, alpha=1, sim=TRUE){
    y <- b0 + b1*x + sim * rnorm(n=length(x), sd=alpha)
    return(y)
  }
)
