#' Logistic Regression for Dichotomous Dependent Variables
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-logit.html}
#' @import methods
#' @export Zelig-logit
#' @exportClass Zelig-logit
#' 
#' @include model-zelig.R
#' @include model-gee.R
#' @include model-gamma.R
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
    .self$packageauthors <- "R Core Team"
    .self$wrapper <- "logit"
  }
)


zlogit$methods(
  mcfun = function(x, b0=0, b1=1, ..., sim=TRUE){
    mu <- 1/(1 + exp(-b0 - b1 * x))
    if(sim){
        y <- rbinom(n=length(x), size=1, prob=mu)
        return(y)
    }else{
        return(mu)
    }
  }
)


zlogit$methods(
  show = function(show.odds.ratios = FALSE,...) {
  if (show.odds.ratios & !.self$mi & !.self$bootstrap) {
    summ <- .self$zelig.out %>%
        do(summ = {cat("Model: \n")
           ## Replace coefficients with odds-ratios
          .z.out.summary = base::summary(.$z.out)
          .z.out.summary$coefficients[,c(1,2)] <- exp(.z.out.summary$coefficients[,c(1,2)])
          colnames(.z.out.summary$coefficients)[c(1,2)] <- paste(colnames(.z.out.summary$coefficients)[c(1,2)], '(odds ratio)')
          print(.z.out.summary)
        })
  }
  else {
    callSuper(...)
  }
    #print(base::summary(.self$zelig.out))
  }
)
