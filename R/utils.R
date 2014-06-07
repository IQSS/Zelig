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

avg <- function(val) {
  if (is.numeric(val))
    mean(val)
  else if (is.ordered(val))
    Median(val)
  else
    Mode(val)
}

setfactor <- function (fv, v) {
  lev <- levels(fv)
  if (!v %in% lev)
    stop("Wrong factor")
  return(factor(v, levels = lev))
}

set <- function(val, newval) {
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

statmat <- function(qi){
  m <- t(apply(qi, 2, quantile, c(.5, .025, .975), na.rm = TRUE))
  n <- matrix(apply(qi, 2, mean, na.rm = TRUE))
  colnames(n) <- "mean"
  o <- matrix(apply(qi, 2, sd, na.rm = TRUE))
  colnames(o) <- "sd"
  p <- cbind(n, o, m)
  return(p)
}

statlevel <- function(qi, num){
  if (is.matrix(qi))
    m <- t(apply(qi, 2, table)) / num
  else
    m <- table(qi) / num
  return(m)
}

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

