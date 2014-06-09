z <- setRefClass("Zelig", fields = list(fn = "ANY", # R function to call
                                        formula = "formula", # Zelig formula
                                        weights = "numeric", 
                                        name = "character", # name of the Zelig model
                                        data = "ANY", # data frame or matrix
                                        data.by = "ANY", # data frame or matrix
                                        by = "logical",
                                        
                                        zelig.call = "call", # Zelig function call
                                        model.call = "call", # wrapped function call
                                        zelig.out = "ANY", # estimated zelig model
                                        zelig.out.by = "list", # list of estimated zelig models
                                        
                                        setx.out = "list", # set values
                                        setx.labels = "list", # pretty-print qi
                                        setx.out.by = "list", # list of set values
                                        
                                        sim.out = "list", # simulated qi's
                                        simparam = "matrix", # simulated parameters
                                        num = "numeric", # nb of simulations
                                        sim.out.by = "list", # list of simulated qi's
                                        simparam.by = "list", # list of simulated parameters
                                        
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
    .self$zelig.out <- eval(.self$model.call, envir = parent.frame(1))
    if (.self$by) {
      .self$data.by <- split(.self$data, factor(.self$data[[by]]))
      .self$zelig.out.by <- list()
      for (i in seq(.self$data.by)) {
        model.call.by <- .self$model.call
        model.call.by$data <- quote(.self$data.by[[i]]) # names(z5$data.by)
        .self$zelig.out.by[[i]] <- eval(model.call.by)
        .self$zelig.out.by[[i]]$call <- names(z5$data.by)[i]
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
        pred <- terms(.self$zelig.out, "predvars")
        n <- intersect(as.character(attr(pred, "predvars"))[-1],
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
    f <- update(formula(.self$zelig.out), 1 ~ .)
    if (!.self$by) {
      ldata <- reduce(.self$data)
      return(model.matrix(f, ldata))
    } else {
      mm <- list()
      for (i in seq(.self$data.by)) {
        ldata <- reduce(.self$data.by[[i]])
        mm[[i]] <- model.matrix(f, ldata)
      }
      names(mm) <- names(.self$data.by)
      return(mm)
    }
  }
)

z$methods(
  setx = function(...) {
    what <- ifelse(!.self$by, "setx.out", "setx.out.by")
    .self[[what]]$x <- .self$set(...)
  }
)

z$methods(
  setx1 = function(...) {
    what <- ifelse(!.self$by, "setx.out", "setx.out.by")
    .self[[what]]$x1 <- .self$set(...)
  }
)

z$methods(
  setrange = function(...) {
    what <- ifelse(!.self$by, "setx.out", "setx.out.by")
    .self[[what]]$range <- list()
    s <- list(...)
    m <- expand.grid(s)
    for (i in 1:nrow(m)) {
      l <- as.list(as.list(m[i, ]))
      names(l) <- names(m)
      .self[[what]]$range[[i]] <- .self$set(l)
    }
  }
)

z$methods(
  sim = function(num = 1000) {
    .self$num <- num
    if (!.self$by) {
      .self$simparam <- .self$param(num = .self$num, z.out = .self$zelig.out)
      if (!is.null(.self$setx.out$x)) {
        .self$sim.out$x <- .self$qi(.self$setx.out$x)
        if (!is.null(.self$setx.out$x1)) {
          .self$sim.out$x1 <- .self$qi(.self$setx.out$x1)
          .self$sim.out$fd <- .self$sim.out$x1$ev - .self$sim.out$x$ev
        }
      } else if (!is.null(.self$setx.out$range)) {
        .self$sim.out$range <- list()
        for (i in seq(.self$setx.out$range)) {
          .self$sim.out$range[[i]] <- .self$qi(.self$setx.out$range[[i]])
        }
      }
    } else {
      for (i in seq(.self$data.by)) {
        .self$simparam.by[[i]] <- .self$param(num = .self$num, z.out = .self$zelig.out.by[[i]])
        if (!is.null(.self$setx.out.by$x[[i]]))
          .self$sim.out.by$x[[i]] <- .self$qi(.self$simparam.by[[i]], .self$setx.out.by$x[[i]])
        if (!is.null(.self$setx.out.by$x1[[i]])) {
          .self$sim.out.by$x1[[i]] <- .self$qi(.self$simparam.by[[i]], .self$setx.out.by$x1[[i]])
          #           for (i in seq(.self$data.by))
          #             .self$sim.out.by$fd[[i]] <- .self$sim.out.by$x1[[i]]$ev - .self$sim.out.by$x[[i]]$ev
          #           names(.self$sim.out.by$fd) <- names(.self$data.by)
        }
        else if (!is.null(.self$setx.out.by$range)) {
          i.self$sim.out.by$range <- list()
          for (i in seq(.self$data.by)) {
            .self$sim.out.by$range[[i]] <- lapply(.self$setx.out.by$range[[i]],
                                                  .self$qi)
          }
        }
      }
    }
  }
)

z$methods(
  summarize = function() {
    cat("Model: ", .self$name, "\n")
    cat("Number of simulations:", .self$num, "\n")
    cat("\nValues of X\n")
    if (!.self$by) {
      if (!is.null(.self$setx.out$x)) {
        print("X")
        print(.self$setx.out$x)
        print(lapply(.self$sim.out$x, stat))
        if (!is.null(.self$setx.out$x1)) {
          print("X1")
          print(.self$setx.out$x1)
          print(lapply(.self$sim.out$x1, stat))
          statmat(.self$sim.out$fd)
        }
      } else if (!is.null(.self$setx.out$range)) {
        print("Range")
        for (i in seq(.self$setx.out$range)) {
          print(.self$setx.out$range[[i]])
          print(lapply(.self$sim.out$range[[i]], stat))
        }
      }
    } else {
      print("by")
      if (!is.null(.self$setx.out.by$x)) {
        for (i in seq(.self$data.by)) {
          print(names(.self$data.by)[[i]])
          print(.self$setx.out.by$x[[i]])
          print(lapply(.self$sim.out.by$x[[i]], stat))
          if (!is.null(.self$setx.out.by$x1[[i]])) {
            print(.self$setx.out.by$x1[[i]])
            print(lapply(.self$sim.out.by$x1[[i]], stat))
          }
        }
      } else if (!is.null(.self$setx.out.by$range)) {
        print("Range")
        for (i in seq(.self$setx.out.by$range)) {
          lapply(.self$sim.out.by$range[[i]], stat)
        }
      }
    }
  }
)

# .self$sim.out.by$x1
# names(.self$sim.out.by$x1)
# .self$sim.out.by$x1[[1]][[1]]
# .self$sim.out.by$x1[[2]][[1]]

# z$methods(
#   summarize = function() {
#     cat("Model: ", .self$name, "\n")
#     cat("Number of simulations:", .self$num, "\n")
#     cat("\nValues of X\n")
#     if (!is.null(.self$setx.out$x)) {
#       print(.self$setx.out)
#       lqi <- .self$sim.out
#       labqi <- names(lqi)
#       for (i in 1:length(lqi)) {
#         qi <- lqi[[i]]
#         if (length(qi) == 1 & is.na(qi[1]))
#           next
#         else if (length(qi) >= 1) {
#           cat("\n", labqi[i], "\n", sep="")
#           if (is.null(attr(qi, "levels")))
#             print(statmat(qi))
#           else
#             print(statlevel(qi, .self$num))
#         }
#       }
#     } else {
#       print("range")
#       for (i in seq(.self$setx.out$range)) {
#         print(i)
#         print(.self$setx.out$range[[i]])
#         lqi <- .self$sim.out
#         labqi <- names(lqi)
#         for (i in 1:length(lqi)) {
#           qi <- lqi[[i]]
#           if (length(qi) == 1 & is.na(qi[1]))
#             next
#           else if ((l <- length(qi)) >= 1) {
#             for (j in i:l) {
#             cat("\n", labqi[i], "\n", sep="")
#             if (is.null(attr(qi, "levels")))
#               print(statmat(qi[[j]]))
#             else
#               print(statlevel(qi[[j]], .self$num))
#             }
#           }
#         }
#       }
#     }
#   }
# )

z$methods(
  summarise = function() {
    .self$summarize()
  }
)

z$methods(
  show = function() {
    if (!.self$by)
      print(summary(.self$zelig.out))
    else
      lapply(z5$zelig.out.by, function(x) print(summary(x)))
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
    .self$ljson <- .self$json
    .self$json <- jsonlite::toJSON(json, pretty = TRUE)
    return(.self$json)
  }
)

#       idx <- match(names(.self$setx.labels),
#                    names(.self$sim.out),
#                    nomatch = 0) 
#       names(.self$sim.out)[idx] <- .self$setx.labels[idx != 0]