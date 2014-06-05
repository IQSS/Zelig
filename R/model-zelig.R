z <- setRefClass("Zelig", fields = list(fn = "ANY", # R function to call
                                        formula = "formula", # Zelig formula
                                        weights = "numeric", 
                                        model = "character", # name of the Zelig model
                                        data = "ANY", # data frame or matrix
                                        
                                        zelig.call = "call", # Zelig function call
                                        model.call = "call", # wrapped function call
                                        zelig.out = "ANY", # estimated zelig model
                                        
                                        qi.out = "list",
                                        setx.out = "list", # set values
                                        setx.labels = "list",
                                        
                                        sim.out = "list", # simulated qi's
                                        simparam = "matrix", # simulated parameters
                                        num = "numeric", # nb of simulations
                                        
                                        authors = "character", # Zelig model description
                                        year = "numeric",
                                        text = "character",
                                        url = "character",
                                        category = "character",
                                        
                                        json = "ANY", # JSON export
                                        ljson = "ANY",
                                        outcome = "ANY",
                                        explanatory = "ANY"
                                        ))

z$methods(
  initialize = function() { 
    .self$authors <- "Kosuke Imai, Gary King, and Olivia Lau"
    .self$year <- as.numeric(format(Sys.Date(), "%Y"))
    .self$url <- "http://gking.harvard.edu/zelig"
    .self$qi.out <- list()
    .self$setx.out <- list()
    .self$setx.labels <- list(ev = "Expected Values: E(Y|X)",
                              ev1 = "Expected Values: E(Y|X1)",
                              pv = "Predicted Values: Y|X",
                              pv1 = "Predicted Values: Y|X1",
                              fd = "First Differences: E(Y|X1) - E(Y|X)")
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
    title <- paste(.self$model, ": ", .self$text, sep="")
    cat("How to cite this model in Zelig:\n  ",
        .self$authors, ". ", .self$year, ".\n  ", title,
        "\n  in Kosuke Imai, Gary King, and Olivia Lau, ",
        "\"Zelig: Everyone's Statistical Software,\"",
        "\n  ", .self$url, "\n", sep = "")
  }
)

z$methods(
  zelig = function(formula, data, ..., weights = NULL) {
    .self$formula <- formula
    .self$data <- data
    .self$model.call <- match.call(expand.dots = TRUE,
                                   call = sys.call(sys.parent(n = )))
    .self$model.call[[1]] <- .self$fn
  }
)

z$methods(
  set = function(...) {
#     n <- all.vars(.self$formula[[3]], .self$data)
#     n <- attr(model.matrix(.self$zelig.out), "dimnames")[[2]]
#     n <- names(model.frame(.self$zelig.out))
#     n <- intersect(names(.self$data), names(model.frame(.self$zelig.out)))
#     n <- c(intersect(names(.self$data), names(model.frame(.self$zelig.out))),
#            all.vars(.self$formula[[3]]))
    pred <- terms(.self$zelig.out, "predvars")
    n <- intersect(as.character(attr(pred, "predvars"))[-1],
                   names(.self$data))
    s <-list(...)
#     print(s)
    if (length(s) > 0 && is.list(s[[1]]))
      s <- s[[1]]
#     print(s)
    m <- match(names(s), n)
    ma <- m[!is.na(m)]
    if (!all(complete.cases(m))) {
      w <- paste("Variable '", names(s[is.na(m)]),
                 "' not in data set.\n", sep = "")
      warning(w)
    }
    if (!all(complete.cases(m)) & length(m) == 0) {
      ldata <- list()
      ldata[n[ma]] <- s[n[ma]]
    } else if (all(is.na(m))) {
      ldata <- lapply(.self$data, avg)
    } else {
      ldata <- lapply(.self$data[n[-ma]], avg)
      ldata[n[ma]] <- s[n[ma]]
    }
    cc <- attr(pred, "dataClasses")
    nn <- names(ldata)
    for (i in seq(nn))
      if (cc[nn[i]] == "factor") {
        if (!(ldata[[nn[i]]] %in% levels(.self$data[[nn[i]]]))) {
          w <- paste("Factor variable '", nn[i],
                     "' has no level '", ldata[[nn[i]]], "'.\n", sep = "")
          warning(w)
          ldata[[nn[i]]] <- avg(.self$data[[nn[i]]])
        } else {
          ldata[[nn[i]]] <- factor(ldata[[nn[i]]],
                                   levels = levels(.self$data[[nn[i]]]))
        }
      }
      f <- update(formula(.self$zelig.out), 1 ~ .)
#     f <- formula(as.Formula(f), rhs = 1)
#     f[[2]] <- 1
    return(model.matrix(f, ldata))
  }
)

z$methods(
  setx = function(...) {
    .self$setx.out$x <- .self$set(...)
  }
)

z$methods(
  setx1 = function(...) {
    .self$setx.out$x1 <- .self$set(...)
  }
)

z$methods(
  setrange = function(...) {
    .self$setx.out$range <- list()
    s <- list(...)
    m <- expand.grid(s)
    for (i in 1:nrow(m))
      .self$setx.out$range[[i]] <- .self$set(as.list(m[i, ]))
  }
)

# z$methods(
#   qi = function(...) {
#     if (!is.null(.self$setx.out$x)) {
#       .self$qi.out$ev <- .self$ev("x")
#       .self$qi.out$pv <- .self$pv("x")
#       if (!is.null(.self$setx.out$x1)) {
#         .self$qi.out$ev1 <- .self$ev("x1")
#         .self$qi.out$pv1 <- .self$pv("x1")
#         .self$qi.out$fd <- .self$qi.out$ev1 - .self$qi.out$ev
#       }
#     }
#     else if (!is.null(.self$setx.out$range)) {
#       print("range")
#       .self$qi.out$ev <- .self$ev("range")
#       .self$qi.out$pv <- .self$pv("range")
# #       for (i in seq(.self$setx.out$range)) {
# #         .self$qi.out$ev[[i]] <- .self$ev("range", i)
# #         .self$qi.out$pv[[i]] <- .self$pv("x")
# #       }
#     }
#     idx <- match(names(.self$setx.labels), names(.self$qi.out), nomatch = 0) 
#     names(.self$qi.out)[idx] <- .self$setx.labels[idx != 0]
#   }
# )

z$methods(
  sim = function(num = 1000) {
    .self$num <- num
    .self$param(num = .self$num)
    .self$qi.out <- list()
    if (!is.null(.self$setx.out$x)) {
      l1 <- .self$qi(.self$simparam, .self$setx.out$x)
      .self$sim.out$ev <- l1[[1]]
      .self$sim.out$pv <- l1[[2]]
      if (!is.null(.self$setx.out$x1)) {
        l2 <- .self$qi(.self$simparam, .self$setx.out$x1)
        .self$sim.out$ev1 <- l2[[1]]
        .self$sim.out$pv1 <- l2[[2]]
        .self$sim.out$fd <- .self$sim.out$ev1 - .self$sim.out$ev
      }
    } else if (!is.null(.self$setx.out$range)) {
      .self$sim.out$range <- list()
      for (i in seq(.self$setx.out$range)) {
        .self$sim.out$range[[i]] <- list()
        lr <- .self$qi(.self$simparam, .self$setx.out$range[[i]])
        .self$sim.out$range[[i]]$ev <- lr[[1]]
        .self$sim.out$range[[i]]$pv <- lr[[2]]
      }
    }
    idx <- match(names(.self$setx.labels), names(.self$sim.out), nomatch = 0) 
    names(.self$sim.out)[idx] <- .self$setx.labels[idx != 0]
  }
)

z$methods(
  summarize = function() {
    cat("Model: ", .self$model, "\n")
    cat("Number of simulations:", .self$num, "\n")
    cat("\nValues of X\n")
    if (!is.null(.self$setx.out$x)) {
      print(.self$setx.out)
      lqi <- .self$sim.out
      labqi <- names(lqi)
      for (i in 1:length(lqi)) {
        qi <- lqi[[i]]
        if (length(qi) == 1 & is.na(qi[1]))
          next
        else if (length(qi) >= 1) {
          cat("\n", labqi[i], "\n", sep="")
          if (is.null(attr(qi, "levels")))
            print(statmat(qi))
          else
            print(statlevel(qi, .self$num))
        }
      }
    } else {
      print("range")
      for (i in seq(.self$setx.out$range)) {
        print(i)
        print(.self$setx.out$range[[i]])
        lqi <- .self$sim.out
        labqi <- names(lqi)
        for (i in 1:length(lqi)) {
          qi <- lqi[[i]]
          if (length(qi) == 1 & is.na(qi[1]))
            next
          else if ((l <- length(qi)) >= 1) {
            for (j in i:l) {
            cat("\n", labqi[i], "\n", sep="")
            if (is.null(attr(qi, "levels")))
              print(statmat(qi[[j]]))
            else
              print(statlevel(qi[[j]], .self$num))
            }
          }
        }
      }
    }
  }
)

z$methods(
  summarise = function() {
    .self$summarize()
  }
)

z$methods(
  show = function() {
    print(summary(.self$zelig.out))
  }
)

z$methods(
  toJSON = function() {
    if (!is.list(.self$json))
      .self$json <- list()
    .self$json$"name" <- .self$model
    .self$json$"description" <- .self$text
    .self$json$"outcome" <- list(modelingType = .self$outcome)
    .self$json$"explanatory" <- list(modelingType = .self$explanatory)
    .self$ljson <- .self$json
    .self$json <- jsonlite::toJSON(json, pretty = TRUE)
    return(.self$json)
  }
)


