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
  v <- sample(names(which(tab == max(tab))), size=1)
  # if it came in as a factor, we need to re-cast it
  # as a factor, with the same exact levels
  if (is.factor(x))
    return(factor(v, levels = levels(x)))
  # re-cast as any other data-type
  as(v, class(x))
}

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

#' Calculate the reduced dataset to be used in code{\link{setx}}
#' 
#' #' This method is used internally
#' 
#' @param dataset Zelig object data, possibly split to deal with \code{by} argument
#' @param s list of variables and their tentative \code{setx} values
#' @param formula a simplified version of the Zelig object formula (typically with 1 on the lhs)
#' @param data Zelig object data
#' @return a list of all the model variables either at their central tendancy or their \code{setx} value
#' @export
#' @keywords internal
#' @author Christine Choirat
reduce <- function(dataset, s, formula, data) {
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

# reduce <- function(dataset, s, model = zelig.out$model[[1]]) {
#   dataset <- as.data.frame(dataset)
#   ldata <- lapply(dataset, avg)
#   if (length(s) > 0) {
#     pred <- terms(model, "predvars")
# #     pred <- terms(model, "regressors")
#     n <- union(as.character(attr(pred, "predvars"))[-1],
#                names(dataset))
# #     n <- union(as.character(attr(pred, "variables"))[-1],
# #                names(dataset))
#     if (is.list(s[[1]]))
#       s <- s[[1]]
#     m <- match(names(s), n)
#     ma <- m[!is.na(m)]
#     if (!all(complete.cases(m))) {
#       w <- paste("Variable '", names(s[is.na(m)]),
#                  "' not in data set.\n", sep = "")
#       warning(w)
#     }
#     for (i in seq(n[ma]))
#       ldata[n[ma]][i][[1]] <- setval(dataset[n[ma]][i][[1]],
#                                      s[n[ma]][i][[1]])
#   }
#   return(ldata)
# }


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

