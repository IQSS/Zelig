#' @include model-zelig.R
ztimeseries <- setRefClass("Zelig-timeseries",
                    contains = "Zelig",
                    fields = list(link = "character",
                                  linkinv = "function"))


ztimeseries$methods(
  initialize = function() {
    callSuper()
    .self$packageauthors <- ""  # Need to decide
    .self$acceptweights <- FALSE  #  Need to deal with block bootstrap
    .self$category <- "timeseries"
    .self$setx.labels <- list(ev  = "Expected Values: E(Y|X)",
                              ev1 = "Expected Values: E(Y|X1)",
                              pv  = "Predicted Values: Y|X",
                              pv1 = "Predicted Values: Y|X1",
                              fd  = "First Differences: E(Y|X1) - E(Y|X)",
                              acf = "Autocorrelation Function",
                              ev.shortrun = "Expected Values Immediately Resulting from Shock",
                              ev.longrun = "Long Run Expected Values after Innovation",
                              pv.shortrun = "Predicted Values Immediately Resulting from Shock",
                              pv.longrun = "Long Run Predicted Values after Innovation",
                              evseries.shock = "Expected Values Over Time from Shock",
                              evseries.innovation ="Expected Values Over Time from Innovation",
                              pvseries.shock = "Predicted Values Over Time from Shock",
                              pvseries.innovation ="Predicted Values Over Time from Innovation")
  }
)

ztimeseries$methods(
  zelig = function(formula, data, ..., weights=NULL, by=NULL){
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    callSuper(formula = formula, data = data, ..., weights = weights, by = by)
  }
)

ztimeseries$methods(
  param = function(z.out) {
    return(mvrnorm(.self$num, coef(z.out), vcov(z.out)))
  }
)

# replace simx method to add ACF as QI.

ztimeseries$methods(
  simx = function() {
    d <- zeligPlyrMutate(.self$zelig.out, simparam = .self$simparam$simparam)
    d <- zeligPlyrMutate(d, mm = .self$setx.out$x$mm)
    .self$sim.out$x <-  d %>%
      do(qi = .self$qi(.$simparam, .$mm)) %>%
      do(acf = .$qi$acf, ev = .$qi$ev, pv = .$qi$pv)
  }
)

ztimeseries$methods(
  simx1 = function() {
    d <- zeligPlyrMutate(.self$zelig.out, simparam = .self$simparam$simparam)
    d <- zeligPlyrMutate(d, mm = .self$setx.out$x$mm)
    d <- zeligPlyrMutate(d, mm1 = .self$setx.out$x1$mm)

#      return(list(acf = acf, ev = ev, pv = pv, pv.shortrun=pv.shortrun, pv.longrun=pv.longrun, ev.shortrun=ev.shortrun, ev.longrun=ev.longrun, 
#                pvseries.shock=yseries$y.shock, pvseries.innovation=yseries$y.innovation,
#                evseries.shock=yseries$ev.shock, evseries.innovation=yseries$ev.innovation))

    .self$sim.out$x1 <-  d %>%
      do(qi = .self$qi(.$simparam, .$mm, .$mm1)) %>%
      do(acf = .$qi$acf, ev = .$qi$ev, pv = .$qi$pv, ev.shortrun = .$qi$ev.shortrun, pv.shortrun = .$qi$pv.shortrun, ev.longrun = .$qi$ev.longrun, pv.longrun = .$qi$pv.longrun, pvseries.shock = .$qi$pvseries.shock, evseries.shock = .$qi$evseries.shock, pvseries.innovation = .$qi$pvseries.innovation,  evseries.innovation = .$qi$evseries.innovation)
      # Will eventually have to then move acf, ev, and pv from .self$setx.out$x1 to .self$setx.out$x
      # This will also effect next line:

    d <- zeligPlyrMutate(.self$sim.out$x1, ev0 = .self$sim.out$x1$ev)    # Eventually, when ev moves, then this path for ev0 changes.  (Or make movement happen after fd calculation.)
    d <- d %>%
      do(fd = .$ev.longrun - .$ev0)
    .self$sim.out$x1 <- zeligPlyrMutate(.self$sim.out$x1, fd = d$fd) #JH
  }
)

# replace sim method to skip {simx, simx1, simrange, simrange1} methods as they are not separable
# instead go directly to qi method

ztimeseries$methods(
  sim = function(num = 1000) {
    "Timeseries Method for Computing and Organizing Simulated Quantities of Interest"
    if (length(.self$num) == 0) 
      .self$num <- num
    .self$simparam <- .self$zelig.out %>%
      do(simparam = .self$param(.$z.out))

    # NOTE difference here from standard Zelig approach.  
    # Normally these are done in sequence, but now we do one or the other.  
    if (.self$bsetx1){
      .self$simx1()
    }else{
      .self$simx()
    }
  }
)