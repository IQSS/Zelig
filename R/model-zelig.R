# library(dplyr)
# library(plyr)

#' A class description
#'
#' @import methods
#' @export Zelig
#' @exportClass Zelig
z <- setRefClass("Zelig", fields = list(fn = "ANY", # R function to call to wrap
                                        formula = "ANY", # Zelig formula
                                        weights = "numeric", 
                                        name = "character", # name of the Zelig model
                                        data = "ANY", # data frame or matrix,
                                        ddata = "ANY",
                                        data.by = "ANY", # data frame or matrix
                                        by = "ANY",
                                        mi = "logical",
                                        
                                        idx = "ANY", # model index
                                        
                                        zelig.call = "call", # Zelig function call
                                        model.call = "call", # wrapped function call
                                        zelig.out = "ANY", # estimated zelig model(s)
                                        
                                        setx.out = "ANY", # set values
                                        setx.labels = "list", # pretty-print qi,
                                        bsetx = "logical",
                                        bsetx1 = "logical",
                                        bsetrange = "logical",
                                        bsetrange1 = "logical",
                                        range = "ANY",
                                        range1 = "ANY",

                                        test.statistics = "ANY",
                                        
                                        sim.out = "list", # simulated qi's
                                        simparam = "ANY", # simulated parameters
                                        num = "numeric", # nb of simulations
                                        
                                        # summary.out = "list", # summary of estimated models
                                        
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
    .self$bsetrange1 <- FALSE
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
    # MI datasets from Amelia
    if(class(data)=="amelia"){
        if(data$m>1){
            .self$mi <- TRUE
            imputationNumber <- rep(1,nrow(data$imputations[[1]]))
            temp<-as.data.frame(cbind(imputationNumber, data$imputations[[1]]))  # check exactly when type cast is necessary
            names(temp)[]<- "imputationNumber"
            for(i in 2:data$m){
                imputationNumber<- rep(i,nrow(data$imputations[[i]]))
                temp<-rbind(as.data.frame(cbind(imputationNumber,data$imputations[[i]])))
            }
            .self$by <- "imputationNumber"  # need to correct when "by" already declared.
            .self$data <- temp
        }else{
            .self$data <- as.data.frame(data$imputations[[1]])
        }
    }else{
      .self$data <- data
    }
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
    f <- update(.self$formula, 1 ~ .)
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
  }
)

z$methods(
  setx1 = function(...) {
    .self$bsetx1 <- TRUE
    .self$setx.out$x1 <- .self$set(...)
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
      .self$setx.out$range[[i]] <- .self$set(l)
    }
  }
)

z$methods(
  setrange1 = function(...) {
    .self$bsetrange1 <- TRUE
    rng <- list()
    s <- list(...)
    m <- expand.grid(s)
    .self$range1 <- m
    .self$setx.out$range1 <- list()
    for (i in 1:nrow(m)) {
      l <- as.list(as.list(m[i, ]))
      names(l) <- names(m)
      .self$setx.out$range1[[i]] <- .self$set(l)
    }
  }
)

z$methods(
  sim = function(num = 1000) {
    .self$num <- num
    .self$simparam <- .self$zelig.out %>%
      do(simparam = .self$param(.$z.out))
    if (.self$bsetx)
      .self$simx()
    if (.self$bsetx1)
      .self$simx1()
    if (.self$bsetrange)
      .self$simrange()
    if (.self$bsetrange1)
      .self$simrange1()
  }
)

z$methods(
  simx = function() {
    d <- plyr::mutate(.self$zelig.out, simparam = .self$simparam$simparam)
    d <- plyr::mutate(d, mm = .self$setx.out$x$mm)
    .self$sim.out$x <-  d %>%
      do(qi = .self$qi(.$simparam, .$mm)) %>%
      do(ev = .$qi$ev, pv = .$qi$pv)
  }
)

z$methods(
  simx1 = function() {
    d <- plyr::mutate(.self$zelig.out, simparam = .self$simparam$simparam)
    d <- plyr::mutate(d, mm = .self$setx.out$x1$mm)
    .self$sim.out$x1 <-  d %>%
      do(qi = .self$qi(.$simparam, .$mm)) %>%
      do(ev = .$qi$ev, pv = .$qi$pv)
    d <- plyr::mutate(.self$sim.out$x1, ev0 = .self$sim.out$x$ev)
    d <- d %>%
      do(fd = .$ev - .$ev0)
    .self$sim.out$x1 <- plyr::mutate(.self$sim.out$x1, fd = d$fd) #JH
  }
)

z$methods(
  simrange = function() {
    .self$sim.out$range <- list()
    for (i in 1:nrow(.self$range)) {
      d <- plyr::mutate(.self$zelig.out, simparam = .self$simparam$simparam)
      d <- plyr::mutate(d, mm = .self$setx.out$range[[i]]$mm)
      .self$sim.out$range[[i]] <-  d %>%
        do(qi = .self$qi(.$simparam, .$mm)) %>%
        do(ev = .$qi$ev, pv = .$qi$pv)
    }
  }
)

z$methods(
  simrange1 = function() {
    .self$sim.out$range1 <- list()
    for (i in 1:nrow(.self$range1)) {
      d <- plyr::mutate(.self$zelig.out, simparam = .self$simparam$simparam)
      d <- plyr::mutate(d, mm = .self$setx.out$range1[[i]]$mm)
      .self$sim.out$range1[[i]] <-  d %>%
        do(qi = .self$qi(.$simparam, .$mm)) %>%
        do(ev = .$qi$ev, pv = .$qi$pv)
    }
  }
)

z$methods(
  show = function() {
    if ("uninitializedField" %in% class(.self$zelig.out))
      cat("Next step: Use 'zelig' method")
    else if (length(.self$setx.out) == 0) {
      summ <- .self$zelig.out %>%
        do(summ = {cat("Model: ")
                   cat(unlist(.[.self$by]))
                   print(.$z.out)})
      cat("Next step: Use 'setx' method\n")
    } else if (length(.self$setx.out) != 0 & length(.self$sim.out) == 0) {
      cat("setx:\n")
      print(.self$setx.out$x$mm)
      cat("setx1:\n")
      print(.self$setx.out$x1$mm)
      cat("setrange:\n")
      print(.self$setx.out$range[[1]]$mm)
      cat("setrange1:\n")
      print(.self$setx.out$range1[[1]]$mm)
      cat("Next step: Use 'sim' method\n")
    } else { # sim.out
      pstat <- function(s.out, what = "sim x") {
        simu <- s.out %>%
          do(simu = {cat("\n", what, ":\n")
                     cat(" -----\n")
                     cat("ev\n")
                     print(stat(.$ev, .self$num))
                     cat("pv\n")
                     print(stat(.$pv, .self$num))
                     if (!is.null(.$fd)) {
                       cat("fd\n")
                       print(stat(.$fd, .self$num))}
          }
          )
      }
      pstat(.self$sim.out$x)
      pstat(.self$sim.out$x1, "sim x1")
      if (!is.null(.self$setx.out$range)) {
        for (i in seq(.self$sim.out$range)) {
          cat("\n")
          print(.self$range[i, ])
          cat("\n")
          pstat(.self$sim.out$range[[i]], "sim range")
          cat("\n")
        }
      }
      if (!is.null(.self$setx.out$range1)) {
        for (i in seq(.self$sim.out$range1)) {
          cat("\n")
          print(.self$range1[i, ])
          cat("\n")
          pstat(.self$sim.out$range1[[i]], "sim range")
          cat("\n")
        }
      }
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
  graph = function() {
    plot.qi(.self)
  }
)

z$methods(
  summarize = function(...) {
    show(...)
  }
)

z$methods(
  summarise = function(...) {
    summarize(...)
  }
)

z$methods(
  help = function() {
    vignette(class(.self)[1])
  }
)

setMethod("summary", "Zelig",
          function(object, ...) {
            object$summarize()
          }
)

setMethod("plot", "Zelig",
          function(x, ...) {
            x$graph()
          }
)

#       idx <- match(names(.self$setx.labels),
#                    names(.self$sim.out),
#                    nomatch = 0) 
#       names(.self$sim.out)[idx] <- .self$setx.labels[idx != 0]
