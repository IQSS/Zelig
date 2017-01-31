#' Compute the Statistical Mode of a Vector
#' @aliases Mode mode
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
## This enables backward compatibility, but results in a warning when library attached
# mode <- Mode

#' Compute the Statistical Median of a Vector
#' @param x a vector of numeric or ordered values
#' @param na.rm ignored
#' @return the median of the vector
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
Median <- function (x, na.rm=NULL) {
  v <- ifelse(is.numeric(x),
              median(x),
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
#'
#' @source Hadley Wickham (2011). The Split-Apply-Combine Strategy for Data
#' Analysis. Journal of Statistical Software, 40(1), 1-29. URL
#' \url{http://www.jstatsoft.org/v40/i01/}.
#' @keywords internal
zelig_mutate <- function (.data, ...)
{
    stopifnot(is.data.frame(.data) || is.list(.data) || is.environment(.data))
    cols <- as.list(substitute(list(...))[-1])
    cols <- cols[names(cols) != ""]
    for (col in names(cols)) {
        .data[[col]] <- eval(cols[[col]], .data, parent.frame())
    }
    .data
}

#' Convenience function for setrange and setrange
#'
#' @param x data passed to setrange or setrange1
#' @keywords internal

expand_grid_setrange <- function(x) {
    m <- expand.grid(x)
    if (nrow(m) == 1)
        warning('Only one fitted observation provided to setrange.\nConsider using setx instead.',
                call. = FALSE)
    return(m)
}

#' Bundle Multiply Imputed Data Sets into an Object for Zelig
#'
#' This object prepares multiply imputed data sets so they can be used by
#'   \code{zelig}.
#' @note This function creates a list of \code{data.frame} objects, which
#'   resembles the storage of imputed data sets in the \code{amelia} object.
#' @param ... a set of \code{data.frame}'s
#' @return an \code{mi} object composed of a list of data frames.
#' @author Matt Owen, James Honaker, and Christopher Gandrud
#' @export
#' @examples
#' # create datasets
#' n <- 100
#' x1 <- runif(n)
#' x2 <- runif(n)
#' y <- rnorm(n)
#' data.1 <- data.frame(y = y, x = x1)
#' data.2 <- data.frame(y = y, x = x2)
#'
#' # merge datasets into one object as if imputed datasets
#'
#' mi.out <- to_zelig_mi(data.1, data.2)
#'
#' # pass object in place of data argument
#' z.out <- zelig(y ~ x, model = "ls", data = mi.out)

to_zelig_mi <- function (...) {

    # Get arguments as list
    imputations <- list(...)
    names(imputations) <- paste("imp", 1:length(imputations), sep = "")

      # Ensure that everything is data.frame
    for (k in length(imputations):1) {
        if (!is.data.frame(imputations[[k]])){
            imputations[[k]] <- NULL
            warning("Item ", k, " of the provided objects is not a data.frame and will be ignored.\n")
        }
    }

    if(length(imputations) < 1){
        stop("The resulting object contains no data.frames, and as such is not a valid multiple imputation object.",
        call. = FALSE)
      }
    if(length(imputations) < 2){
        stop("The resulting object contains only one data.frame, and as such is not a valid multiple imputation object.",
        call. = FALSE)
    }
    class(imputations) <-c("mi", "list")

    return(imputations)
}
