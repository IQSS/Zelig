zbinchoice <- setRefClass("Zelig-bbinchoice",
                          contains = "Zelig",
                          field = list(family = "ANY",
                                       linkinv = "function"
                          ))

zbinchoice$methods(
  initialize = function() {
    callSuper()
    .self$fn <- quote(VGAM::vglm)
    .self$authors <- "Kosuke Imai, Gary King, Olivia Lau"
    .self$year <- 2007
    .self$category <- "dichotomous"
  }
)

zbinchoice$methods(
  zelig = function(formula, data, ..., weights = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    callSuper(formula = formula, data = data, ..., weights = NULL)
    .self$model.call$family <- .self$family
    .self$zelig.out <- eval(.self$model.call)
  }
)

zbinchoice$methods(
  param = function(num, ...) {
    .self$simparam <- mvrnorm(n = num, mu = coef(.self$zelig.out),
                              Sigma = vcov(.self$zelig.out))
  }
)

zbinchoice$methods(
  # From Zelig 4
  qi = function(x=NULL, param=NULL) {
    .pp <- function(object, constr, all.coef, x) {
      xm <- list()
      xm <- rep(list(NULL), 3)
      sim.eta <- NULL
      # again this can definitely be written better
      for (i in 1:length(constr))
        for (j in 1:3)
          if (sum(constr[[i]][j,]) == 1)
            xm[[j]] <- c(xm[[j]], x[,names(constr)[i]])
      sim.eta <- cbind(
        all.coef[[1]] %*% as.matrix( xm[[1]] ),
        all.coef[[2]] %*% as.matrix( xm[[2]] ),
        all.coef[[3]] %*% as.matrix( xm[[3]] )
      )
      # compute inverse (theta)
      ev <- .self$linkinv(sim.eta)
      # assign correct column names
      colnames(ev) <- c("Pr(Y1=0, Y2=0)",
                        "Pr(Y1=0, Y2=1)",
                        "Pr(Y1=1, Y2=0)",
                        "Pr(Y1=1, Y2=1)"
      )
      return(ev)
    }
    
    .pr <- function(ev) {
      mpr <- cbind(ev[, 3] + ev[, 4], ev[, 2] + ev[, 4])
      index <- matrix(NA, ncol=2, nrow=nrow(mpr))
      index[, 1] <- rbinom(n=nrow(ev), size=1, prob=mpr[, 1])
      index[, 2] <- rbinom(n=nrow(ev), size=1, prob=mpr[, 2])
      pr <- matrix(NA, nrow=nrow(ev), ncol=4)
      pr[, 1] <- as.integer(index[, 1] == 0 & index[, 2] == 0)
      pr[, 2] <- as.integer(index[, 1] == 0 & index[, 2] == 1)
      pr[, 3] <- as.integer(index[, 1] == 1 & index[, 2] == 0)
      pr[, 4] <- as.integer(index[, 1] == 1 & index[, 2] == 1)
      colnames(pr) <- c("(Y1=0, Y2=0)",
                        "(Y1=0, Y2=1)",
                        "(Y1=1, Y2=0)",
                        "(Y1=1, Y2=1)")
      return(pr)
    }
    .make.match.table <- function(index, cols=NULL) {
      pr <- matrix(0, nrow=nrow(index), ncol=4)
      # assigns values by the rule:
      #   pr[j,1] = 1 iff index[j,1] == 0 && index[j,2] == 0
      #   pr[j,2] = 1 iff index[j,1] == 0 && index[j,2] == 1
      #   pr[j,3] = 1 iff index[j,1] == 1 && index[j,2] == 0
      #   pr[j,4] = 1 iff index[j,1] == 1 && index[j,2] == 1
      # NOTE: only one column can be true at a time, so as a result
      #       we can do a much more elegant one liner, that I'll code
      #       later.  In this current form, I don't think this actually
      #       explains what is going on.
      pr[, 1] <- as.integer(index[, 1] == 0 & index[, 2] == 0)
      pr[, 2] <- as.integer(index[, 1] == 0 & index[, 2] == 1)
      pr[, 3] <- as.integer(index[, 1] == 1 & index[, 2] == 0)
      pr[, 4] <- as.integer(index[, 1] == 1 & index[, 2] == 1)
      # assign column names
      colnames(pr) <- if (is.character(cols) && length(cols)==4)
        cols
      else
        c("(Y1=0, Y2=0)",
          "(Y1=0, Y2=1)",
          "(Y1=1, Y2=0)",
          "(Y1=1, Y2=1)")
      return(pr)
    }
    all.coef <- NULL
    coefs <- .self$simparam
    cm <- constraints(.self$zelig.out)
    v <- vector("list", 3)
    for (i in 1:length(cm)) {
      if (ncol(cm[[i]]) == 1){
        for (j in 1:3)
          if (sum(cm[[i]][j, ]) == 1)
            v[[j]] <- c(v[[j]], names(cm)[i])
      }
      else {
        for (j in 1:3)
          if (sum(cm[[i]][j,]) == 1)
            v[[j]] <- c(v[[j]], paste(names(cm)[i], ":", j, sep=""))
      }
    }
    for(i in 1:3)
      all.coef[[i]] <- coefs[ , unlist(v[i]) ]
    col.names <- c("Pr(Y1=0, Y2=0)",
                   "Pr(Y1=0, Y2=1)",
                   "Pr(Y1=1, Y2=0)",
                   "Pr(Y1=1, Y2=1)"
    )
    ev <- .pp(.self$zelig.out, cm, all.coef, as.matrix(x))
    pr <- .pr(ev)
    levels(pr) <- c(0, 1)
    return(list("Predicted Probabilities: Pr(Y1=k|X)" = ev,
                "Predicted Values: Y=k|X" = pr))
  }
)

zbinchoice$methods(
  show = function(...) {
    print(VGAM::summary(.self$zelig.out, ...))
  }
)

