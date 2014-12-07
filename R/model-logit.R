#' @include model-zelig.R
#' @include model-glm.R
#' @include model-binchoice.R
zlogit <- setRefClass("Zelig-logit",
                      contains = "Zelig-binchoice")
  
zlogit$methods(
  initialize = function() {
    callSuper()
    .self$name <- "logit"
    .self$link <- "logit"
    .self$description = "Logistic Regression for Dichotomous Dependent Variables"
    .self$wrapper <- "logit"
  }
)

zlogit$methods(
  test = function(b0 = -1, b1 = 1, nsim = 1000, minx = -1, maxx = 1) {
    x.init <- mcunit.init(nsim, minx, maxx)
    
    b0 <- b0
    b1 <- b1 
    sd <- 1
    
    pi.sim <- 1 / (1 + exp(-b1 * x.init[,1] - b0))
    y.sim <- rbinom(nsim, 1, pi.sim)
    y.true <- 1 / (1 + exp(-b1 * x.init[,2] -b0))
    data <- cbind(x.init, y.sim, y.true)
    
    z <- zlogit$new()
    callSuper(z, data)
    
  }
)
