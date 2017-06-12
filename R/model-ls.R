#' Least Squares Regression for Continuous Dependent Variables
#'
#' @param formula a symbolic representation of the model to be
#'   estimated, in the form \code{y \~\, x1 + x2}, where \code{y} is the
#'   dependent variable and \code{x1} and \code{x2} are the explanatory
#'   variables, and \code{y}, \code{x1}, and \code{x2} are contained in the
#'   same dataset. (You may include more than two explanatory variables,
#'   of course.) The \code{+} symbol means ``inclusion'' not
#'   ``addition.'' You may also include interaction terms and main
#'   effects in the form \code{x1*x2} without computing them in prior
#'   steps; \code{I(x1*x2)} to include only the interaction term and
#'   exclude the main effects; and quadratic terms in the form
#'   \code{I(x1^2)}.
#' @param model the name of a statistical model to estimate.
#'   For a list of supported models and their documentation see:
#'   \url{http://docs.zeligproject.org/articles/}.
#' @param data the name of a data frame containing the variables
#'   referenced in the formula or a list of multiply imputed data frames
#'   each having the same variable names and row numbers (created by
#'   \code{Amelia} or \code{\link{to_zelig_mi}}).
#' @param ... additional arguments passed to \code{zelig},
#'   relevant for the model to be estimated.
#' @param by a factor variable contained in \code{data}. If supplied,
#'   \code{zelig} will subset
#'   the data frame based on the levels in the \code{by} variable, and
#'   estimate a model for each subset. This can save a considerable amount of
#'   effort. For example, to run the same model on all fifty states, you could
#'   use: \code{z.out <- zelig(y ~ x1 + x2, data = mydata, model = 'ls',
#'   by = 'state')} You may also use \code{by} to run models using MatchIt
#'   subclasses.
#' @param cite If is set to 'TRUE' (default), the model citation will be printed
#'   to the console.
#'
#' @details
#' Additional parameters avaialable to many models include:
#' \itemize{
#'   \item weights: vector of weight values or a name of a variable in the dataset
#'   by which to weight the model. For more information see:
#'   \url{http://docs.zeligproject.org/articles/weights.html}.
#'   \item bootstrap: logical or numeric. If \code{FALSE} don't use bootstraps to
#'   robustly estimate uncertainty around model parameters due to sampling error.
#'   If an integer is supplied, the number of boostraps to run.
#'   For more information see:
#'   \url{http://docs.zeligproject.org/articles/bootstraps.html}.
#' }

#'
#' @return Depending on the class of model selected, \code{zelig} will return
#'   an object with elements including \code{coefficients}, \code{residuals},
#'   and \code{formula} which may be summarized using
#'   \code{summary(z.out)} or individually extracted using, for example,
#'   \code{coef(z.out)}. See
#'   \url{http://docs.zeligproject.org/articles/getters.html} for a list of
#'   functions to extract model components. You can also extract whole fitted
#'   model objects using \code{\link{from_zelig_model}}.
#'
#' @examples
#' data(macro)
#' z.out1 <- zelig(unem ~ gdp + capmob + trade, model = "ls", data = macro)
#'
#' @seealso Vignette: \url{http://docs.zeligproject.org/articles/zelig_ls.html}
#' @import methods
#' @export Zelig-ls
#' @exportClass Zelig-ls
#'
#' @include model-zelig.R

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
  zelig = function(formula, data, ..., weights = NULL, by = NULL,
      bootstrap = FALSE) {
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
      return(list(simparam = mvrnorm(.self$num, coef(z.out), vcov(z.out)),
                  simalpha = rep( summary(z.out)$sigma, .self$num) )  )
    } else if(identical(method,"point")){
      return(list(simparam = t(as.matrix(coef(z.out))),
             simalpha=summary(z.out)$sigma))
    } else {
      stop("param called with method argument of undefined type.")
    }
  }
)

zls$methods(
  qi = function(simparam, mm) {
    ev <- simparam$simparam %*% t(mm)
    pv <- as.matrix(rnorm(n=length(ev), mean=ev, sd=simparam$simalpha),
                    nrow=length(ev), ncol=1)
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
