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

#' Check if simulations are present in sim.out
#' @param x a sim.out method
#' @param fail logical whether to return an error if no simulations are present.

is_simspresent <- function(x, fail = TRUE) {
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
