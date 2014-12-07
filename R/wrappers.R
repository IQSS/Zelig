zelig <- function(formula, model, data, ..., by = NULL, cite = TRUE) {
  #   .Deprecated("\nz$new() \nz$zelig(...)")
  zeligmodels <- system.file(file.path("JSON", "zelig5models.json"), package = "Zelig")
  models <- jsonlite::fromJSON(txt = readLines(zeligmodels))$zelig5models
  
  zeligchoicemodels <- system.file(file.path("JSON", "zelig5choicemodels.json"), package = "ZeligChoice")
  if (zeligchoicemodels != "")
    models <- c(models, jsonlite::fromJSON(txt = readLines(zeligchoicemodels))$zelig5choicemodels)
  
  models4 <- list()
  for (i in seq(models)) {
    models4[[models[[i]]$wrapper]] <- names(models)[i]
  }
  
  model.init <- paste0("z", models4[[model]], "$new()")
  z5 <- try(eval(parse(text = model.init)), silent = TRUE)
  if ("try-error" %in% class(z5))
    stop("Model '", model,"' not found")
  ## End: Zelig 5 models (more to be linked from, e.g, Zelig5Choice)
  mf <- match.call()
  mf$model <- NULL
  mf$cite <- NULL
  mf[[1]] <- quote(z5$zelig)
  mf <- try(eval(mf, environment()), silent = TRUE)
  if ("try-error" %in% class(mf))
    z5$zelig(formula = formula, data = data, ..., by = by)
  if (cite)
    z5$cite()
  return(z5)
}

setx <- function(obj, fn = NULL, data = NULL, cond = FALSE, ...) {
  # .Deprecated("\nz$new() \nz$zelig(...) \nz$setx() or z$setx1 or z$setrange")
  x5 <- obj$copy()
  # This is the length of each argument in '...'s
  s <- list(...)
  if (length(s) > 0) {
    hold <- rep(1, length(s))
    for(i in 1:length(s)) {
      hold[i] <- length(s[i][[1]])
    }
  } else {
    hold <- 1
  }
  if (max(hold) > 1) {
    x5$setrange(...)
  } else {
    x5$setx(...)
  }
  return(x5)
}

sim <- function(obj, x = NULL, x1 = NULL, y = NULL, num = 1000, bootstrap = F, 
                bootfn = NULL, cond.data = NULL, ...) {
  # .Deprecated("\nz$new() \n[...] \nz$sim(...)")
  s5 <- x$copy()
  if (!is.null(x1)) {
    s15 <- x1$copy()
    if (!is.null(s15$setx.out$x)) {
      s5$setx.out$x1 <- s15$setx.out$x
      s5$bsetx1 <- TRUE
    }
    if (!is.null(s15$setx.out$range)) {
      s5$range1<-s15$range
      s5$setx.out$range1 <- s15$setx.out$range
      s5$bsetrange1 <- TRUE
    }
  }
  s5$sim(num = num)
  return(s5)
}
