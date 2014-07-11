library(dplyr)
library(plyr)

#' A class description
#'
#' @import methods
#' @export Zelig
#' @exportClass Zelig
z <- setRefClass("Zelig", fields = list(fn = "ANY", # R function to call to wrap
                                        formula = "formula", # Zelig formula
                                        weights = "numeric", 
                                        name = "character", # name of the Zelig model
                                        data = "ANY", # data frame or matrix,
                                        ddata = "ANY",
                                        data.by = "ANY", # data frame or matrix
                                        by = "ANY",
                                        
                                        idx = "ANY", # model index
                                        
                                        zelig.call = "call", # Zelig function call
                                        model.call = "call", # wrapped function call
                                        zelig.out = "ANY", # estimated zelig model(s)
                                        
                                        setx.out = "ANY", # set values
                                        setx.labels = "list", # pretty-print qi,
                                        bsetx = "logical",
                                        bsetx1 = "logical",
                                        bsetrange = "logical",
                                        range = "ANY",
                                        
                                        sim.out = "list", # simulated qi's
                                        simparam = "ANY", # simulated parameters
                                        num = "numeric", # nb of simulations
                                        
                                        authors = "character", # Zelig model description
                                        year = "numeric",
                                        description = "character",
                                        url = "character",
                                        category = "character",
                                        
                                        json = "ANY", # JSON export
                                        ljson = "ANY",
                                        outcome = "ANY",
                                        explanatory = "ANY"))

z$methods(
  initialize = function() { 
    .self$authors <- "Kosuke Imai, Gary King, and Olivia Lau"
    .self$year <- as.numeric(format(Sys.Date(), "%Y"))
    .self$url <- "http://datascience.iq.harvard.edu/zelig"
    .self$setx.out <- list()
    .self$setx.labels <- list(ev  = "Expected Values: E(Y|X)",
                              ev1 = "Expected Values: E(Y|X1)",
                              pv  = "Predicted Values: Y|X",
                              pv1 = "Predicted Values: Y|X1",
                              fd  = "First Differences: E(Y|X1) - E(Y|X)")
    .self$bsetx <- FALSE
    .self$bsetx1 <- FALSE
    .self$bsetrange <- FALSE
    # JSON
    .self$explanatory <- c("continuous",
                           "discrete",
                           "nominal",
                           "ordinal",
                           "binary")
    .self$outcome <- ""
  }
)

z$methods(
  cite = function() {
    title <- paste(.self$name, ": ", .self$description, sep="")
    cat("How to cite this model in Zelig:\n  ",
        .self$authors, ". ", .self$year, ".\n  ", title,
        "\n  in Kosuke Imai, Gary King, and Olivia Lau, ",
        "\"Zelig: Everyone's Statistical Software,\"",
        "\n  ", .self$url, "\n", sep = "")
  }
)

z$methods(
  zelig = function(formula, data, ..., weights = NULL, by) {
    fn2 <- function(fc, data) {
      fc$data <- data
      return(fc)
    }
    .self$formula <- formula
    .self$by <- by
    .self$data <- data
    .self$model.call[[1]] <- .self$fn
    .self$model.call$by <- NULL
    if (is.null(.self$by)) {
      .self$data <- cbind(1, .self$data)
      names(.self$data)[1] <- "by"
      .self$by <- "by"
    }
    .self$data <- tbl_df(.self$data)
    .self$zelig.out <- .self$data %>% 
      regroup(lapply(.self$by, as.symbol)) %>% 
      do(z.out = eval(fn2(.self$model.call, quote(.))))
  }
)

z$methods(
  set = function(...) {
    s <-list(...)
    f <- update(formula(.self$zelig.out$z.out[[1]]), 1 ~ .)
    update <- .self$data %>% 
      regroup(lapply(.self$by, as.symbol)) %>% 
      do(mm = model.matrix(f, reduce(dataset = ., s, 
                       model = .self$zelig.out$z.out[[1]])))
    return(update)
  }
)

z$methods(
  setx = function(...) {
    .self$bsetx <- TRUE
    .self$setx.out$x  <- .self$set(...)
#     names(x)[names(x) == "mm"] <- "mmx"
#     .self$setx.out <- x
#     .self$zelig.out <- mutate(.self$zelig.out, mmx = .self$setx.out$mmx)
  }
)

z$methods(
  setx1 = function(...) {
    .self$bsetx1 <- TRUE
    .self$setx.out$x1 <- .self$set(...)
#     names(x1)[names(x1) == "mm"] <- "mmx1"
#     .self$setx.out <- inner_join(.self$setx.out, x1)
#     .self$zelig.out <- mutate(.self$zelig.out, mmx1 = .self$setx.out$mmx1)
  }
)

z$methods(
  setrange = function(...) {
    .self$bsetrange <- TRUE
    rng <- list()
    s <- list(...)
    m <- expand.grid(s)
    .self$range <- m
    .self$setx.out$range <- list()
    for (i in 1:nrow(m)) {
      l <- as.list(as.list(m[i, ]))
      names(l) <- names(m)
#       x <- .self$set(l)
#       names(x)[names(x) == "mm"] <- paste("mmr", i, sep = "")
      .self$setx.out$range[[i]] <- .self$set(l)
#       if (i == 1)
#         .self$setx.out <- x
#       else
#         .self$setx.out <- inner_join(.self$setx.out, x)
    }
  }
)

z$methods(
  sim = function(num = 1000) {
    .self$num <- num
    .self$simparam <- .self$zelig.out %>%
      do(simparam = .self$param(.$z.out))
#     .self$zelig.out <- mutate(.self$zelig.out,
#                               simparam =.self$simparam$simparam)
    if (.self$bsetx)
      .self$simx()
    if (.self$bsetx1)
      .self$simx1()
    if (.self$bsetrange)
      .self$simrange()
  }
)

z$methods(
  simx = function() {
    d <- mutate(.self$zelig.out, simparam = .self$simparam$simparam)
    d <- mutate(d, mm = .self$setx.out$x$mm)
    .self$sim.out$x <-  d %>%
      do(qi = .self$qi(.$simparam, .$mm)) %>%
      do(ev = .$qi$ev, pv = .$qi$pv)
  }
)

z$methods(
  simx1 = function() {
    d <- mutate(.self$zelig.out, simparam = .self$simparam$simparam)
    d <- mutate(d, mm = .self$setx.out$x1$mm)
    .self$sim.out$x1 <-  d %>%
      do(qi = .self$qi(.$simparam, .$mm)) %>%
      do(ev = .$qi$ev, pv = .$qi$pv)
#     .self$zelig.out <- mutate(.self$zelig.out, ev1 = .self$sim.out$ev1)
#     .self$zelig.out <- mutate(.self$zelig.out, pv1 = .self$sim.out$pv1)
#     .self$zelig.out <- mutate(.self$zelig.out, fd = .self$sim.out$fd)
  }
)

z$methods(
  simrange = function() {
    .self$sim.out$range <- list()
    for (i in 1:nrow(.self$range)) {
      d <- mutate(.self$zelig.out, simparam = .self$simparam$simparam)
      d <- mutate(d, mm = .self$setx.out$range[[i]]$mm)
      .self$sim.out$range[[i]] <-  d %>%
        do(qi = .self$qi(.$simparam, .$mm)) %>%
        do(ev = .$qi$ev, pv = .$qi$pv)
    }
  }
)

z$methods(
  show = function() {
    if (length(.self$zelig.out) == 0)
      print("Use 'zelig' method")
    else if (length(.self$setx.out) == 0)
      print("Use 'setx' method")
    else {
      summ <- .self$zelig.out %>%
        do(summ = summary(.$z.out))
      .self$zelig.out <- mutate(.self$zelig.out, summ = summ$summ)
    }
  }
)

z$methods(
  toJSON = function() {
    if (!is.list(.self$json))
      .self$json <- list()
    .self$json$"name" <- .self$name
    .self$json$"description" <- .self$description
    .self$json$"outcome" <- list(modelingType = .self$outcome)
    .self$json$"explanatory" <- list(modelingType = .self$explanatory)
    .self$json$tree <- c(class(.self)[1],
                         head(.self$.refClassDef@refSuperClasses, -1))
    .self$ljson <- .self$json
    .self$json <- jsonlite::toJSON(json, pretty = TRUE)
    return(.self$json)
  }
)

z$methods(
  help = function() {
    vignette(class(.self)[1])
  }
)

setMethod('summary', "Zelig",
          function(object, ...) {
            object$summarize()
          }
)

#       idx <- match(names(.self$setx.labels),
#                    names(.self$sim.out),
#                    nomatch = 0) 
#       names(.self$sim.out)[idx] <- .self$setx.labels[idx != 0]
