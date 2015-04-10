#' Probit Regression for Dichotomous Dependent Variables
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-probit.html}
#' @import methods
#' @export Zelig-probit
#' @exportClass Zelig-probit
#'
#' @include model-zelig.R
#' @include model-glm.R
#' @include model-binchoice.R
  
zprobit <- setRefClass("Zelig-probit",
                       contains = "Zelig-binchoice")

zprobit$methods(
  initialize = function() {
    callSuper()
    .self$name <- "probit"
    .self$link <- "probit"
    .self$description = "Probit Regression for Dichotomous Dependent Variables"
    .self$packageauthors <- "R Core Team"
    .self$wrapper <- "probit"
  }
)

zprobit$methods(
  test = function(b0 = -1, b1 = 1, nsim = 1000, minx = -1, maxx = 1) {
    x.init <- mcunit.init(nsim, minx, maxx)
    
    b0 <- b0
    b1 <- b1 
    sd <- 1
    
    pi.sim <- pnorm(b1 * x.init[,1] + b0) 
    y.sim <- rbinom(nsim, 1, pi.sim)
    y.true <- pnorm(b1 * x.init[,2] + b0)
    data.local <- cbind(x.init, y.sim, y.true)
    
    z <- zprobit$new()
    callSuper(z, data.local)
    
  }
)




