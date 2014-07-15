#' @include model-zelig.R
library(Formula)
zivareg <- setRefClass("Zelig-ivareg", contains = "Zelig",
                       fields = list(vcov = "ANY"))

zivareg$methods(
  initialize = function() {
    callSuper()
    .self$name <- "ivareg"
    .self$year <- 2014
    .self$authors <- "Ryan Kennedy"
    .self$category <- "continuous"
    .self$description <- "Instrumental Variables Estimation, ivreg from AER package"
    .self$fn <- quote(AER::ivreg)
    .self$vcov <- sandwich
    # JSON
    .self$outcome <- "continous"
  }
)

zivareg$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)
    callSuper(formula = formula, data = data, ...,
              weights = NULL, by = by)
    .self$formula <- as.Formula(.self$formula)
  }
)

zivareg$methods(
  param = function(z.out) {
    return(mvrnorm(.self$num, coef(z.out), summary(z.out, vcov=.self$vcov)$vcov))
  }
)

zivareg$methods(
  qi = function(simparam, mm) {
    ev <- simparam %*% t(mm)
    pv <- ev
    return(list(ev = ev, pv = pv))
  }
)

