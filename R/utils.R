#' Compute the Statistical Mode of a Vector
#' @param x a vector of numeric, factor, or ordered values
#' @return the statistical mode of the vector. If two modes exist, one is
#'   randomly selected (by design)
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
Mode <- function (x) {
  # build a table of values of x
  tab <- table(as.factor(x))
  # find the mode, then if there's more than one, select one randomly
  v <- sample(names(which(tab == max(tab))), size = 1)
  # if it came in as a factor, we need to re-cast it
  # as a factor, with the same exact levels
  if (is.factor(x))
    return(factor(v, levels = levels(x)))
  # re-cast as any other data-type
  as(v, class(x))
}

## Zelig 3 and 4 backward compatibility
mode <- Mode

#' Compute the Statistical Median of a Vector
#' @param x a vector of numeric or ordered values
#' @param na.rm ignored
#' @return the median of the vector
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
Median <- function (x, na.rm=NULL) {
  v <- ifelse(is.numeric(x),
              median(v),
              levels(x)[ceiling(median(as.numeric(x)))]
  )
  if (is.ordered(x))
    v <- factor(v, levels(x))
  v
}

#' Create a table, but ensure that the correct
#' columns exist. In particular, this allows for
#' entires with zero as a value, which is not
#' the default for standard tables
#' @param x a vector
#' @param levels a vector of levels
#' @param ... parameters for table
#' @return a table
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
table.levels <- function (x, levels, ...) {
  # if levels are not explicitly set, then
  # search inside of x
  if (missing(levels)) {
    levels <- attr(x, 'levels')
    table(factor(x, levels=levels), ...)
  }
  # otherwise just do the normal thing
  else {
    table(factor(x, levels=levels), ...)
  }
}

#' Compute central tendancy as approrpriate to data type
#' @param val a vector of values
#' @return a mean (if numeric) or a median (if ordered) or mode (otherwise)
#' @export
avg <- function(val) {
  if (is.numeric(val))
    mean(val)
  else if (is.ordered(val))
    Median(val)
  else
    Mode(val)
}

#' Set new value of a factor variable, checking for existing levels
#' @param fv factor variable
#' @param v value
#' @return a factor variable with a value \code{val} and the same levels
#' @keywords internal
setfactor <- function (fv, v) {
  lev <- levels(fv)
  if (!v %in% lev)
    stop("Wrong factor")
  return(factor(v, levels = lev))
}

#' Set new value of a variable as approrpriate to data type
#' @param val old value
#' @param newval new value
#' @return a variable of the same type with a value \code{val}
#' @keywords internal
setval <- function(val, newval) {
  if (is.numeric(val))
    newval
  else if (is.ordered(val))
    newval
  else {
    lev <- levels(val)
    if (!newval %in% lev)
      stop("Wrong factor")
    return(factor(newval, levels = lev))
  }
} 

#' Calculate the reduced dataset to be used in code{\link{setx}}
#' 
#' #' This method is used internally
#' 
#' @param dataset Zelig object data, possibly split to deal with \code{by} argument
#' @param s list of variables and their tentative \code{setx} values
#' @param formula a simplified version of the Zelig object formula (typically with 1 on the lhs)
#' @param data Zelig object data
#' @param avg function of data transformations
#' @return a list of all the model variables either at their central tendancy or their \code{setx} value
#' @export
#' @keywords internal
#' @author Christine Choirat
reduce = function(dataset, s, formula, data, avg = avg) {
  pred <- try(terms(fit <- lm(formula, data), "predvars"), silent = TRUE)
  if ("try-error" %in% class(pred)) # exp and weibull
    pred <- try(terms(fit <- survreg(formula, data), "predvars"), silent = TRUE)
  dataset <- model.frame(fit)
  ldata <- lapply(dataset, avg)
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
  return(ldata)
}

#' Describe Here
#' @param qi quantity of interest in the discrete case
#' @return a formatted qi
#' @keywords internal
#' @author Christine Choirat
statmat <- function(qi) {
  m <- t(apply(qi, 2, quantile, c(.5, .025, .975), na.rm = TRUE))
  n <- matrix(apply(qi, 2, mean, na.rm = TRUE))
  colnames(n) <- "mean"
  o <- matrix(apply(qi, 2, sd, na.rm = TRUE))
  colnames(o) <- "sd"
  p <- cbind(n, o, m)
  return(p)
}

#' Describe Here
#' @param qi quantity of interest in the discrete case
#' @param num number of simulations
#' @return a formatted qi
#' @keywords internal
#' @author Christine Choirat
statlevel <- function(qi, num) {
    if (is.matrix(qi)){
        #m <- t(apply(qi, 2, table)) / num
        all.levels <- levels(qi)
        m <- t(apply(qi, 2, function(x) table(factor(x, levels=all.levels)))) / num
    }else{
        m <- table(qi) / num
    }
  return(m)
}

#' Pass Quantities of Interest to Appropriate Summary Function
#' 
#' @param qi quantity of interest (e.g., estimated value or predicted value)
#' @param num number of simulations
#' @return a formatted qi
#' @keywords internal
#' @author Christine Choirat
stat <- function(qi, num) {
  if (is.null(attr(qi, "levels")))
    return(statmat(qi))
  else
    return(statlevel(qi, num))
}

#' Generate Formulae that Consider Clustering
#'
#' This method is used internally by the "Zelig" Package to interpret
#' clustering in GEE models.
#' @param formula a formula object
#' @param cluster a vector
#' @return a formula object describing clustering
cluster.formula <- function (formula, cluster) { 
  # Convert LHS of formula to a string
  lhs <- deparse(formula[[2]])
  cluster.part <- if (is.null(cluster))
    # NULL values require
    sprintf("cluster(1:nrow(%s))", lhs)
  else
    # Otherwise we trust user input
    sprintf("cluster(%s)", cluster)
  update(formula, paste(". ~ .", cluster.part, sep = " + "))
}


#' Zelig Copy of plyr::mutate to avoid namespace conflict with dplyr
#' @keywords internal
zeligPlyrMutate<-function (.data, ...) 
{
    stopifnot(is.data.frame(.data) || is.list(.data) || is.environment(.data))
    cols <- as.list(substitute(list(...))[-1])
    cols <- cols[names(cols) != ""]
    for (col in names(cols)) {
        .data[[col]] <- eval(cols[[col]], .data, parent.frame())
    }
    .data
}

#' List available zelig models.
#' @return A named list. Each element contains information about a zelig class
#' @export
zmodelsAvailable <- function() {
  zeligmodels <- system.file(file.path("JSON", "zelig5models.json"), package = "Zelig")
  models <- jsonlite::fromJSON(txt = readLines(zeligmodels))$zelig5models
  # Zelig Choice
  zeligchoicemodels <- system.file(file.path("JSON", "zelig5choicemodels.json"),
                                   package = "ZeligChoice")
  if (zeligchoicemodels != "")
    models <- c(models, jsonlite::fromJSON(txt = readLines(zeligchoicemodels))$zelig5choicemodels)
  # Zelig Panel
  zeligpanelmodels <- system.file(file.path("JSON", "zelig5panelmodels.json"),
                                   package = "ZeligPanel")
  if (zeligpanelmodels != "")
    models <- c(models, jsonlite::fromJSON(txt = readLines(zeligpanelmodels))$zelig5panelmodels)
  # Zelig GAM
  zeligammodels <- system.file(file.path("JSON", "zelig5gammodels.json"),
                               package = "ZeligGAM")
  if (zeligammodels != "")
    models <- c(models, jsonlite::fromJSON(txt = readLines(zeligammodels))$zelig5gammodels)
  # Zelig Multilevel
  zeligmixedmodels <- system.file(file.path("JSON", "zelig5mixedmodels.json"),
                               package = "ZeligMultilevel")
  if (zeligmixedmodels != "")
    models <- c(models, jsonlite::fromJSON(txt = readLines(zeligmixedmodels))$zelig5mixedmodels)
  # Aggregating all available models
  return(models)
}

#' Match a model fit object to the corresponsding Zelig class.
#'
#' This is a generic function. At the moment the only useful method is for lm objects.
#' @param model A model fit object
#' @param ... Not currently used.
#' @export
zmodelMatcher <- function(model, ...) {
  UseMethod("zmodelMatcher")
}
## zmodelMatcher.lm <- function(model) {
##   return(zls$new())
## }

#' Match a model fit object to the corresponsding Zelig class.
#'
#' This is a generic function. At the moment the only useful method is for lm objects.
#' @param model A model fit object
#' @return a zelig object capable of producing the model.
#' @export
zmodelMatcher.lm <- function(model) {
  zmodels <- lapply(zmodelsAvailable(), unlist, recursive = TRUE)
  model.args <- as.list(getCall(model))
  func <- as.character(model.args[[1]])
  family <- model.args$family
  link <- model.args$family
  if(!is.null(family)) {
    family <- model.args$family
    link <- model.args$link
    linkinv <- model.args$linkinv
    family.obj <- eval(family)
    if(class(family.obj) == "family") {
      family <- family.obj$family
      link <- family.obj$link
      linkinv <- family.obj$linkinv
    }
  }
  zmodelMatch <- na.omit(
    sapply(zmodels,
           function(x) {
             if(is.na(x["func"])) x["func"] <- x["name"]
             if(is.na(x["func"])) x["func"] <- "unknown"
             zmodel <- na.omit(gsub("^.*::", "", x[c("func", "family", "link")]))
             return(all(zmodel == na.omit(c(func, family, link))))
           }))
  nmatches <- sum(zmodelMatch) 
  if(sum(nmatches) < 1) stop("Could not find a matching Zelig model.")
  zmodel <- zmodels[zmodelMatch][[1]]
  if(sum(nmatches) > 1) warning(paste("Multiple models matched, using ",
                                      zmodel[["name"]],
                                      ":",
                                      paste(sapply(zmodels[zmodelMatch],
                                                   function(x) x[["name"]]),
                                            collapse = ", "),
                                      sep = ""))
  return(list(zelig = eval(parse(text = paste("z", zmodel[["name"]], "$new()", sep = ""))),
              model.args = model.args[-1]))
}



