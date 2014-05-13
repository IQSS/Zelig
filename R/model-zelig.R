z <- setRefClass("Zelig", fields = list(fn = "ANY", # R function to wrap, was "call"
                                        formula = "formula", # Zelig formula
                                        weights = "numeric", 
                                        model = "character", # name of the Zelig model
                                        data = "ANY", # data frame or matrix
                                        zelig.call = "call", # Zelig function call
                                        model.call = "call", # wrapped function call
                                        zelig.out = "ANY", # estimated zelig model
                                        setx.out = "matrix", # setx model matrix
                                        sim.out = "list", # simulated qi's
                                        simparam = "matrix", # simulated parameters
                                        num = "numeric", # nb of simulations
                                        
                                        authors = "character", # Zelig model description
                                        year = "numeric",
                                        text = "character",
                                        url = "character",
                                        category = "character",
                                        
                                        json = "ANY" # JSON export
                                        ))

z$methods(
  initialize = function() { 
    .self$authors <- "Kosuke Imai, Gary King, and Olivia Lau"
    .self$year <- as.numeric(format(Sys.Date(), "%Y"))
    .self$url <- "http://gking.harvard.edu/zelig"
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
  setx = function(...) {
    n <<- all.vars(.self$formula[[3]], .self$data)
    s <<- list(...)
    m <<- match(names(s), n)
    ma <<- m[!is.na(m)]
    if (!all(complete.cases(m))) {
      w <- paste("Variable", names(s[is.na(m)]), "not in data set.\n")
      warning(w)
    }
    if (is.na(m)) {
      print("here")
      ldata <- lapply(.self$data, avg)
    } else {
      ldata <- lapply(.self$data[n[-ma]], avg)
      ldata[n[ma]] <- s[n[ma]]
    }
    f <- update(formula(.self$zelig.out), 1 ~ .)
    .self$setx.out <- model.matrix(f, ldata)
  }
)

z$methods(
  sim = function(num = 1000) {
    .self$num <- num
    .self$param(num = .self$num)
    .self$sim.out <- .self$qi(x = .self$setx.out)
  }
)

z$methods(
  summarize = function() {
    cat("Model: ", .self$model, "\n")
    cat("Number of simulations:", .self$num, "\n")
    cat("\nValues of X\n")
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
  }
)

z$methods(
  show = function() {
    print(summary(.self$zelig.out))
  }
)

z$methods(
  toJSON = function() {
    .self$json$"name" <- .self$model
    .self$json$"description" <- .self$text
    .self$json <- toJSON(.self$json, pretty = TRUE)
  }
)

# z$methods(
#   toJSON = function() {
#     json <- list("-name" = .self$model, "-label" = .self$text)
#     .self$json <- jsonlite::toJSON(json, pretty = TRUE)
#   }
# )

