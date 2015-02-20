#' @include model-zelig.R
zcoxph <- setRefClass("Zelig-coxph",
                      contains = "Zelig")

zcoxph$methods(
  initialize = function() {
    callSuper()
    .self$name <- "coxph"
    .self$authors <- "Patrick Lam"
    .self$packageauthors <- "Terry M Therneau, and Thomas Lumley"
    .self$year <- 2007
    .self$description <- "Cox Proportional Hazard Regression for Duration Dependent Variables"
    .self$fn <- quote(survival::coxph)
    # JSON
    .self$outcome <- "bounded"
    .self$wrapper <- "coxph"
  }
)

zcoxph$methods(
  zelig = function(formula, ..., robust = FALSE, cluster = NULL, data, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    if (!(is.null(cluster) || robust))
      stop("If cluster is specified, then `robust` must be TRUE")
    # Add cluster term
    if (robust || !is.null(cluster))
      formula <- cluster.formula(formula, cluster)
    .self$model.call$model <- FALSE
    callSuper(formula = formula, data = data, ..., robust = robust,
              cluster = cluster,  by = by)
  }
)

zcoxph$methods(
  reduce = function(dataset, s, formula, data) {
    dataset <- as.data.frame(dataset)
    ldata <- lapply(dataset, avg)
    pred <- try(terms(coxph(formula, data), "predvars"), silent = TRUE)
    if (length(s) > 0) {
      n <- union(as.character(attr(pred, "predvars"))[-1],
                 names(dataset))
      if (is.list(s[[1]]))
        s <- s[[1]]
      m <- match(names(s), n)
      ma <- m[!is.na(m)]
      if (!all(complete.cases(m))) {
        w <- paste("Variable '", names(s[is.na(m)]),
                   "' not in data set.\n", sep = "")
        warning(w)
      }
      for (i in seq(n[ma]))
        ldata[n[ma]][i][[1]] <- setval(dataset[n[ma]][i][[1]],
                                       s[n[ma]][i][[1]])
    }
    if (!is.null(attr(pred, "specials")$strata)) {
      # Get strata variable names
      st <- names(attr(pred, "dataClasses")[attr(pred, "specials")$strata])
      st <- paste(st, collapse = " ")
      stnames <- gsub("[\\(\\)]", "", regmatches(st, gregexpr("\\(.*?\\)", st))[[1]])
      # Treat st  rata as a factor
      for (stname in stnames)
        ldata[stname] <<- Mode(.self$data[[stname]])
    }
    return(ldata)
  }
)

zcoxph$methods(
  set = function(...) {
    s <- list(...)
    update <- .self$data %>% 
      group_by_(.self$by) %>%
      do(mm = model.matrix(.self$zelig.out$z.out[[1]], # same model matrix across 'by'
                           .self$reduce(dataset = ., s, 
                                        formula = .self$formula,
                                        data = .self$data)))
    return(update)
  }
)

# No param method needed?
zcoxph$methods(
  param = function(z.out) {
    Intercept <- 1
    simparam.local <- cbind(Intercept, mvrnorm(.self$num, coef(z.out), vcov(z.out)))
    return(simparam.local)
  }
)

zcoxph$methods(
  qi = function(z.out, mm) {
    qi <- list()
    surv.fit <- survfit(z.out, newdata = as.data.frame(mm))
    surv <- surv.fit$surv
    time <- surv.fit$time
    surv.se <- summary(surv.fit)$std.err
    log.surv <- log(surv)
    log.surv.se <- surv.fit$std.err
    
    surv.sims <- matrix(NA, nrow = .self$num, ncol = length(surv))
    for (i in 1:length(surv))
      surv.sims[, i] <- exp(rnorm(.self$num, mean = log.surv[i], sd = log.surv.se[i]))
    colnames(surv.sims) <- time
    
    # "Estimated Survival Function Over Time: S(t|X)"
    qi$survival <- surv.sims
    ## Cumulative Hazard
    # "Estimated Cumulative Hazard Over Time: H(t|X)"
    qi$cumhaz <- cumhaz <- -log(surv.sims)
    ## Hazard
    cumhaz.means <- colMeans(cumhaz)
    hazard <- matrix(NA, ncol = ncol(cumhaz), nrow = 1)
    colnames(hazard) <- colnames(cumhaz)
    hazard[, 1] <- cumhaz.means[1]
    for (i in 2:length(time))
      hazard[, i] <- cumhaz.means[i] - cumhaz.means[i - 1]
    # "Estimated Hazard Rate Over Time: h(t|X)"
    qi$hazard <- hazard
    return(qi)
  }
)

zcoxph$methods(
  simx = function() {
    d <- plyr::mutate(.self$zelig.out)
    d <- plyr::mutate(d, mm = .self$setx.out$x$mm)
    .self$sim.out$x <-  d %>%
      do(qi = .self$qi(.$z.out, .$mm)) %>%
      do(ev = .$qi$survival, pv = .$qi$cumhaz, fd = .$qi$hazard)
  }
)

