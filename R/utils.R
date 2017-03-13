#' Compute the Statistical Mode of a Vector
#' @aliases Mode mode
#' @param x a vector of numeric, factor, or ordered values
#' @return the statistical mode of the vector. If more than one mode exists,
#'  the last one in the factor order is arbitrarily chosen (by design)
#' @export
#' @author Christopher Gandrud and Matt Owen

Mode <- function (x) {
    # build a table of values of x
    tab <- table(as.factor(x))
    # find the mode, if there is more than one arbitrarily pick the last
    max_tab <- names(which(tab == max(tab)))
    v <- max_tab[length(max_tab)]
    # if it came in as a factor, we need to re-cast it as a factor, with the same exact levels
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
#' @author Matt Owen

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
#' @author Matt Owen

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
  else if (is.logical(val))
    newval
  else {
    lev <- levels(val)
    if (!newval %in% lev)
      stop("Wrong factor", call. = FALSE)
    return(factor(newval, levels = lev))
  }
}


#' Calculate the reduced dataset to be used in \code{\link{setx}}
#'
#' #' This method is used internally
#'
#' @param dataset Zelig object data, possibly split to deal with \code{by}
#'   argument
#' @param s list of variables and their tentative \code{setx} values
#' @param formula a simplified version of the Zelig object formula (typically
#'   with 1 on the lhs)
#' @param data Zelig object data
#' @param avg function of data transformations
#' @return a list of all the model variables either at their central tendancy or
#'   their \code{setx} value
#'
#' @keywords internal
#' @author Christine Choirat and Christopher Gandrud
#' @export

reduce = function(dataset, s, formula, data, avg = avg) {
    pred <- try(terms(fit <- lm(formula, data), "predvars"), silent = TRUE)
    if ("try-error" %in% class(pred)) # exp and weibull
        pred <- try(terms(fit <- survreg(formula, data), "predvars"),
                    silent = TRUE)

    dataset <- model.frame(fit)

    ldata <- lapply(dataset, avg)
    if (length(s) > 0) {
        n <- union(as.character(attr(pred, "predvars"))[-1], names(dataset))
        if (is.list(s[[1]])) s <- s[[1]]
        m <- match(names(s), n)
        ma <- m[!is.na(m)]
        if (!all(complete.cases(m))) {
            w <- paste("Variable '", names(s[is.na(m)]), "' not in data set.\n",
                     sep = "")
            stop(w, call. = FALSE)
        }
        for (i in seq(n[ma])) {
            ldata[n[ma]][i][[1]] <- setval(dataset[n[ma]][i][[1]],
                                           s[n[ma]][i][[1]])
        }
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
#' @return a formatted quantity of interest
#' @keywords internal
#' @author Christine Choirat
statlevel <- function(qi, num) {
    if (is.matrix(qi)){
        #m <- t(apply(qi, 2, table)) / num
        all.levels <- levels(qi)
        m <- t(apply(qi, 2, function(x)
            table(factor(x, levels=all.levels)))) / num
    } else {
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

#' Convenience function for setrange and setrange1
#'
#' @param x data passed to setrange or setrange1
#' @keywords internal

expand_grid_setrange <- function(x) {
#    m <- expand.grid(x)
    set_lengths <- unlist(lapply(x, length))
    unique_set_lengths <- unique(as.vector(set_lengths))

    m <- data.frame()
    for (i in unique_set_lengths) {
        temp_df <- data.frame(row.names = 1:i)
        for (u in 1:length(x)) {
            if (length(x[[u]]) == i) {
                temp_df <- cbind(temp_df, x[[u]])
                names(temp_df)[ncol(temp_df)] <- names(x)[u]
            }
        }
        if (nrow(m) == 0) m <- temp_df
        else m <- merge(m, temp_df)
    }
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
#' @param ... a set of \code{data.frame}'s or a single list of \code{data.frame}'s
#' @return an \code{mi} object composed of a list of data frames.
#'
#' @author Matt Owen, James Honaker, and Christopher Gandrud
#'
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
#' @export

to_zelig_mi <- function (...) {

    # Get arguments as list
    imputations <- list(...)

    # If user passes a list of data.frames rather than several data.frames as separate arguments
    if((class(imputations[[1]]) == 'list') & (length(imputations) == 1)){
        imputations = imputations[[1]]
    }

    # Labelling
    names(imputations) <- paste0("imp", 1:length(imputations))

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

#' Enables backwards compatability for preparing non-amelia imputed data sets
#' for \code{zelig}.
#'
#' See \code{\link{to_zelig_mi}}
#'
#' @param ... a set of \code{data.frame}'s
#' @return an \code{mi} object composed of a list of data frames.
mi <- to_zelig_mi

#' Conduct variable transformations called inside a \code{zelig} call
#'
#' @param formula model formulae
#' @param data data frame used in \code{formula}
#' @param FUN character string of the transformation function
#' @param check logical whether to just check if a formula contains an
#'   internally called transformation and return \code{TRUE} or \code{FALSE}
#' @param f_out logical whether to return the converted formula
#' @param d_out logical whether to return the converted data frame. Note:
#'   \code{f_out} must be missing
#'
#' @author Christopher Gandrud
#' @internal

transformer <- function(formula, data, FUN = 'log', check, f_out, d_out) {

    if (FUN == 'as.factor') FUN_temp <- 'as\\.factor'
    else FUN_temp <- FUN
    FUN_str <- sprintf('%s\\(', FUN_temp)

    f <- as.character(formula)[3]
    f_split <- unlist(strsplit(f, split = ' '))
    to_transform <- grep(pattern = FUN_str, f_split)

    if (!missing(check)) {
        if (length(to_transform) > 0) return(TRUE)
        else return(FALSE)
    }

    if (length(to_transform) > 0) {
        to_transform_raw <- f_split[to_transform]
        to_transform_plain <- gsub(FUN_str, '', to_transform_raw)
        to_transform_plain <- gsub('\\)', '', to_transform_plain)

        if (!all(to_transform_plain %in% names(data)))
            stop('Unable to find variable to transform.')

        if (!missing(f_out)) {
            f_split[to_transform] <- to_transform_plain
            f_comb <- paste(f_split, collapse = ' ')
            dv <- gsub('\\(\\)', '', formula[2])
            f_new <- paste(dv, '~', f_comb, collapse = ' ')
            f_out <- as.formula(f_new)
            return(f_out)
        }
        else if (d_out) {
            for (i in to_transform_plain)
              data[, i] <- eval(parse(text = sprintf('%s(data[, i])',
                                                     FUN)))
            return(data)
        }
    }
    else if (length(to_transform) == 0) {
        if (!missing(f_out)) return(formula)
        else if (d_out) return(data)
    }
}



#' Remove package names from fitted model object calls.
#'
#' Enables \code{\link{from_zelig_model}} output to work with stargazer.
#' @param x a fitted model object result
#' @internal

strip_package_name <- function(x) {
    call_temp <- gsub('^.*(?=(::))', '', x$call[1], perl = TRUE)
    call_temp <- gsub('::', '', call_temp, perl = TRUE)
    x$call[1] <- as.call(list(as.symbol(call_temp)))
    return(x)
}

#' Extract p-values from a fitted model object
#' @param x a fitted Zelig object
#' @internal

p_pull <- function(x) {
    p_values <- summary(x)$coefficients
    if ('Pr(>|t|)' %in% colnames(p_values)) {
        p_values <- p_values[, 'Pr(>|t|)']
    } else {
        p_values <- p_values[, 'Pr(>|z|)']
    }
    return(p_values)
}

#' Extract standard errors from a fitted model object
#' @param x a fitted Zelig object
#' @internal

se_pull <- function(x) {
    se <- summary(x)$coefficients[, "Std. Error"]
    return(se)
}

#' Drop intercept columns from a data frame of fitted values
#'
#' @param x a data frame
#' @internal

rm_intercept <- function(x) {
    intercept_names <- c('(Intercept)', 'X.Intercept.', '(Intercept).*')
    names_x <- names(x)
    if (any(intercept_names %in% names(x))) {
        keep <- !(names(x) %in% intercept_names)
        x <- data.frame(x[, names_x[keep]])
        names(x) <- names_x[keep]
    }
    return(x)
}


#' Combines estimated coefficients and associated statistics
#' from models estimated with multiply imputed data sets or bootstrapped
#'
#' @param obj a zelig object with an estimated model
#' @param out_type either \code{"matrix"} or \code{"list"} specifying
#'   whether the results should be returned as a matrix or a list.
#' @param bagging logical whether or not to bag the bootstrapped coefficients
#' @param messages logical whether or not to return messages for what is being
#'   returned
#'
#' @return If the model uses multiply imputed or bootstrapped data then a
#'  matrix (default) or list of combined coefficients (\code{coef}), standard
#'  errors (\code{se}), z values (\code{zvalue}), p-values (\code{p}) is
#'  returned. Rubin's Rules are used to combine output from multiply imputed
#'  data. An error is returned if no imputations were included or there wasn't
#'  bootstrapping. Please use \code{get_coef}, \code{get_se}, and
#'  \code{get_pvalue} methods instead in cases where there are no imputations or
#'  bootstrap.
#'
#' @examples
#' set.seed(123)
#'
#' ## Multiple imputation example
#' # Create fake imputed data
#' n <- 100
#' x1 <- runif(n)
#' x2 <- runif(n)
#' y <- rnorm(n)
#' data.1 <- data.frame(y = y, x = x1)
#' data.2 <- data.frame(y = y, x = x2)
#'
#' # Estimate model
#' mi.out <- to_zelig_mi(data.1, data.2)
#' z.out.mi <- zelig(y ~ x, model = "ls", data = mi.out)
#'
#' # Combine and extract coefficients and standard errors
#' combine_coef_se(z.out.mi)
#'
#' ## Bootstrap example
#' z.out.boot <- zelig(y ~ x, model = "ls", data = data.1, bootstrap = 20)
#' combine_coef_se(z.out.boot)
#'
#' @author Christopher Gandrud and James Honaker
#' @source Partially based on \code{\link{mi.meld}} from Amelia.
#'
#' @export

combine_coef_se <- function(obj, out_type = 'matrix', bagging = FALSE,
                            messages = TRUE)
{
    is_zelig(obj)
    is_uninitializedField(obj$zelig.out)
    if (!(out_type %in% c('matrix', 'list')))
        stop('out_type must be either "matrix" or "list"', call. = FALSE)

    if (obj$mi || obj$bootstrap) {
        coeflist <- obj$get_coef()
        vcovlist <- obj$get_vcov()
        coef_names <- names(coeflist[[1]])

        am.m <- length(coeflist)
        if (obj$bootstrap & !obj$mi) am.m <- am.m - 1
        am.k <- length(coeflist[[1]])
        if (obj$bootstrap & !obj$mi)
            q <- matrix(unlist(coeflist[-(am.m + 1)]), nrow = am.m,
                        ncol = am.k, byrow = TRUE)
        else if (obj$mi) {
            q <- matrix(unlist(coeflist), nrow = am.m, ncol = am.k,
                        byrow = TRUE)
            se <- matrix(NA, nrow = am.m, ncol = am.k)
            for(i in 1:am.m){
                se[i, ] <- sqrt(diag(vcovlist[[i]]))
            }
        }
        ones <- matrix(1, nrow = 1, ncol = am.m)
        comb_q <- (ones %*% q)/am.m
        if (obj$mi) ave.se2 <- (ones %*% (se^2)) / am.m
        diff <- q - matrix(1, nrow = am.m, ncol = 1) %*% comb_q
        sq2 <- (ones %*% (diff^2))/(am.m - 1)
        if (obj$mi) {
            if (messages) message('Combining imputations. . .')
            comb_se <- sqrt(ave.se2 + sq2 * (1 + 1/am.m))
            coef <- as.vector(comb_q)
            se <- as.vector(comb_se)
        }

        else if (obj$bootstrap  & !obj$mi) {
            if (messages) message('Combining bootstraps . . .')
            comb_se <- sqrt(sq2 * (1 + 1/am.m))
            if (bagging) {
                coef <- as.vector(comb_q)
            } else {
                coef <- coeflist[[am.m + 1]]
            }
            se <- as.vector(comb_se)
        }

        zvalue <- coef / se
        pr_z <- 2 * (1 - pnorm(abs(zvalue)))

        if (out_type == 'matrix') {
            out <- cbind(coef, se, zvalue, pr_z)
            colnames(out) <- c("Estimate", "Std.Error", "z value", "Pr(>|z|)")
            rownames(out) <- coef_names
        }
        else if (out_type == 'list') {
            out <- list(coef = coef, se = se, zvalue = zvalue, p = pr_z)
            for (i in seq(out)) names(out[[i]]) <- coef_names
        }
        return(out)
    }
    else if (!(obj$mi || obj$bootstrap))
        stop('No multiply imputed or bootstrapped estimates found. So no need to combine.',
            call. = FALSE)
}
