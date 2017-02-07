#' Check if is a zelig object
#' @param x an object
#' @param fail logical whether to return an error if x is not a Zelig object.

is_zelig <- function(x, fail = TRUE) {
    is_it <- inherits(x, "Zelig")
    if (isTRUE(fail)) {
        if(!isTRUE(is_it)) stop('Not a Zelig object.',
                                call. = FALSE)
    } else return(is_it)
}

#' Check if uninitializedField
#' @param x a zelig.out method
#' @param fail logical whether to return an error if x uninitialzed.

is_uninitializedField <- function(x, fail = TRUE) {
    passes <- FALSE
    if (length(x) == 1) passes <- inherits(x, "uninitializedField")

    if (isTRUE(fail)) {
        if (isTRUE(passes))
            stop('Zelig model has not been estimated.', call. = FALSE)
    } else return(passes)
}

#' Check if any simulations are present in sim.out
#' @param x a sim.out method
#' @param fail logical whether to return an error if no simulations are present.

is_sims_present <- function(x, fail = TRUE) {
    passes <- TRUE
    if (is.null(x$x) & is.null(x$range)) passes <- FALSE
    if (isTRUE(fail)) {
        if (!isTRUE(passes))
            stop('No simulated quantities of interest found.', call. = FALSE)
    } else return(passes)
}

#' Check if simulations for individual values are present in sim.out
#' @param x a sim.out method
#' @param fail logical whether to return an error if simulation range is not
#'   present.

is_simsx <- function(x, fail = TRUE) {
    passes <- TRUE
    if (is.null(x$x)) passes <- FALSE
    if (isTRUE(fail)) {
        if (!isTRUE(passes))
            stop('Simulations for individual fitted values are not present.',
                call. = FALSE)
    } else return(passes)
}

#' Check if simulations for a range of fitted values are present in sim.out
#' @param x a sim.out method
#' @param fail logical whether to return an error if simulation range is not
#'   present.

is_simsrange <- function(x, fail = TRUE) {
    passes <- TRUE
    if (is.null(x$range)) passes <- FALSE
    if (isTRUE(fail)) {
        if (!isTRUE(passes))
            stop('Simulations for a range of fitted values are not present.',
                call. = FALSE)
    } else return(passes)
}

#' Check if an object has a length greater than 1
#' @param x an object
#' @param msg character string with the error message to return if
#'   \code{fail = TRUE}.
#' @param fail logical whether to return an error if length is not greater than
#'   1.

is_length_not_1 <- function(x, msg = 'Length is 1.', fail = TRUE) {
    passes <- TRUE

    if (length(x) == 1) passes <- FALSE
    if (isTRUE(fail)) {
        if (!isTRUE(passes))
            stop(msg, call. = FALSE)
    } else return(passes)
}

#' Check if the values in a vector vary
#' @param x a vector
#' @param msg character string with the error message to return if
#'   \code{fail = TRUE}.
#' @param fail logical whether to return an error if \code{x} does not vary.

is_varying <- function(x, msg = 'Vector does not vary.', fail = TRUE) {
    if (!is.vector(x)) stop('x must be a vector.', call. = FALSE)
    passes <- TRUE

    if (length(unique(x)) == 1) passes <- FALSE
    if (isTRUE(fail)) {
        if (!isTRUE(passes))
            stop(msg, call. = FALSE)
    } else return(passes)
}

#' Check if a zelig object contains a time series model
#' 
#' @param x a zelig object
#' @param msg character string with the error message to return if
#'   \code{fail = TRUE}.
#' @param fail logical whether to return an error if \code{x} is not a timeseries.

is_timeseries <- function(x, msg = 'Not a timeseries object.', fail = FALSE) {
    is_zelig(x)
    passes <- TRUE
    if (!"timeseries" %in% x$category) passes <- FALSE
    if (isTRUE(fail)) {
        if (!isTRUE(passes))
            stop(msg, call. = FALSE)
    } else return(passes)
}