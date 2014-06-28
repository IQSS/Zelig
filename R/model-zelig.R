z <- setRefClass("Zelig", fields = list(fn = "ANY", # R function to call
                                        formula = "formula", # Zelig formula
                                        weights = "numeric", 
                                        name = "character", # name of the Zelig model
                                        data = "ANY", # data frame or matrix
                                        data.by = "ANY", # data frame or matrix
                                        by = "logical",
                                        idx = "ANY", # model index
                                        
                                        zelig.call = "call", # Zelig function call
                                        model.call = "call", # wrapped function call
                                        zelig.out = "list", # estimated zelig model(s)
                                        
                                        setx.out = "list", # set values
                                        setx.labels = "list", # pretty-print qi
                                        
                                        sim.out = "list", # simulated qi's
                                        simparam = "list", # simulated parameters
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
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$formula <- formula
    .self$data <- data
    .self$model.call[[1]] <- .self$fn
    .self$by <- !is.null(by)
    .self$model.call$by <- NULL
    .self$idx <- 1
    if (.self$by) {
      .self$data.by <- split(.self$data, factor(.self$data[[by]]))
      .self$idx <- seq(.self$data.by)
    } else if (class(.self$data) == "amelia") {
      .self$data.by <- .self$data$imputations
      .self$by <- TRUE
      .self$idx <- seq(.self$data.by)
    } else {
      .self$data.by <- list()
      .self$data.by[[1]] <- .self$data
      .self$zelig.out[[.self$idx]] <- eval(.self$model.call,
                                           envir = parent.frame(1))
    }
    if (.self$by) {
#       .self$data.by <- split(.self$data, factor(.self$data[[by]]))
#       .self$idx <- seq(.self$data.by)
      for (i in .self$idx) {
        model.call.by <- .self$model.call
        model.call.by$data <- quote(.self$data.by[[i]]) # names(z5$data.by)
        .self$zelig.out[[i]] <- eval(model.call.by)
        .self$zelig.out[[i]]$call <- names(z5$data.by)[i]
      }
    }
  }
)

z$methods(
  set = function(...) {
    s <-list(...)
    reduce <- function(dataset) {
      ldata <- lapply(dataset, avg)
      if (length(s) > 0) {
        pred <- terms(.self$zelig.out[[1]], "predvars")
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
      return(ldata)
    }
    f <- update(formula(.self$zelig.out[[1]]), 1 ~ .)
    mm <- list()
    for (i in .self$idx) {
      ldata <- reduce(.self$data.by[[i]])
      mm[[i]] <- model.matrix(f, ldata)
    }
    names(mm) <- names(.self$data.by)
    return(mm)
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
    for (i in 1:nrow(m)) {
      l <- as.list(as.list(m[i, ]))
      names(l) <- names(m)
      .self$setx.out$range[[i]] <- .self$set(l)
    }
  }
)

z$methods(
  sim = function(num = 1000) {
    .self$num <- num
    for (i in .self$idx) {
      .self$param(i)
      if (!is.null(.self$setx.out$x[[i]])) {
        .self$sim.out$x[[i]] <- .self$qi(i, .self$setx.out$x[[i]])
        names(.self$sim.out$x)[[i]] <- names(.self$data.by)[[i]]
        if (!is.null(.self$setx.out$x1[[i]])) {
          .self$sim.out$x1[[i]] <- .self$qi(i, .self$setx.out$x1[[i]])
          names(.self$sim.out$x1)[[i]] <- names(.self$data.by)[[i]]
          .self$sim.out$fd[[i]] <- .self$sim.out$x1[[i]]$ev - .self$sim.out$x[[i]]$ev
          names(.self$sim.out$fd)[[i]] <- names(.self$data.by)[[i]]
        }
      }
    }
    if (!is.null(.self$setx.out$range)) {
      .self$sim.out$range <- list()
      for (j in seq(.self$setx.out$range)) {
        .self$sim.out$range[[j]] <- list()
        for (i in .self$idx) {
          .self$param(i)
          .self$sim.out$range[[j]][[i]] <- .self$qi(i, .self$setx.out$range[[j]][[i]])
        }
        names(.self$sim.out$range[[j]]) <- names(.self$data.by)
      }
    }
    names(.self$simparam) <- names(.self$data.by)
  }
)

z$methods(
  summarize = function() {
    cat("Model: ", .self$name, "\n")
    cat("Number of simulations:", .self$num, "\n")
    for (i in .self$idx) {
      cat("\nValues of X\n")
      if (!is.null(.self$setx.out$x[[i]])) {
        print(.self$setx.out$x[[i]])
        print(lapply(.self$sim.out$x[[i]], stat, num = .self$num))
        if (!is.null(.self$setx.out$x1[[i]])) {
          cat("\nValues of X1\n")
          print(.self$setx.out$x1[[i]])
          print(lapply(.self$sim.out$x1[[i]], stat, num = .self$num))
          print(stat(.self$sim.out$fd[[i]]))
        }
      }
      if (!is.null(.self$setx.out$range)) {
        for (j in seq(.self$setx.out$range)) {
          cat(paste("\nValues of X: range", j, "\n"))
          for (i in .self$idx) {
            cat("subset: ", names(.self$data.by)[i], "\n")
            print(.self$setx.out$range[[j]][[i]])
            print(lapply(.self$sim.out$range[[j]][[i]], stat, num = .self$num))
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
    if (length(.self$zelig.out) == 0)
      print("Use 'zelig' method")
    else if (length(.self$setx.out) == 0)
      print("Use 'setx' method")
    else
      lapply(.self$zelig.out, function(x) print(summary(x)))
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

# 
#       idx <- match(names(.self$setx.labels),
#                    names(.self$sim.out),
#                    nomatch = 0) 
#       names(.self$sim.out)[idx] <- .self$setx.labels[idx != 0]
