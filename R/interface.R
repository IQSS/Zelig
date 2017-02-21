#' Extract the original fitted model object from a \code{zelig} estimation
#'
#' @param obj a zelig object with an estimated model
#'
#' @details Extracts the original fitted model object from a \code{zelig}
#'   estimation. This can be useful for passing output to non-Zelig
#'   post-estimation functions and packages such as texreg and stargazer
#'   for creating well-formatted presentation document tables.
#'
#' @examples
#' z5 <- zls$new()
#' z5$zelig(Fertility ~ Education, data = swiss)
#' from_zelig_model(z5)
#'
#' @author Christopher Gandrud
#' @export

from_zelig_model <- function(obj) {
  is_zelig(obj)
  
  f5 <- obj$copy()
  return(f5$from_zelig_model())
}

#' Extract simulated quantities of interest from a zelig object
#'
#' @param obj a zelig object with simulated quantities of interest
#'
#' @details Returns simulated quantities of interest in a tidy data formatted
#'   `data.frame`. This can be useful for creating custom plots.
#'
#'  Each row contains a simulated value and each column contains:
#'
#'  - `x` whether the simulations are from the base `x` `setx` or the 
#'      contrasting `x1` for finding first differences.
#'  - The fitted values specified in `setx` including a `by` column if
#'     `by` was used in the \code{\link{zelig}} call.
#'  - `expected_value`
#'  - `expected_value`
#'
#' @examples
#' #### QIs without first difference or range, from covariates fitted at
#' ## central tendencies
#' z.1 <- zelig(Petal.Width ~ Petal.Length + Species, data = iris,
#'              model = "ls")
#' z.1 <- setx(z.1)
#' z.1 <- sim(z.1)
#' zelig_qi_to_df(z.1)
#'
#' #### QIs for first differences
#' z.2 <- zelig(Petal.Width ~ Petal.Length + Species, data = iris,
#'              model = "ls")
#' z.2a <- setx(z.2, Petal.Length = 2)
#' z.2b <- setx(z.2, Petal.Length = 4.4)
#' z.2 <- sim(z.2, x = z.2a, x1 = z.2a)
#' zelig_qi_to_df(z.2)
#'
#' #### QIs for first differences, estimated by Species
#' z.3 <- zelig(Petal.Width ~ Petal.Length, by = "Species", data = iris,
#'              model = "ls")
#' z.3a <- setx(z.3, Petal.Length = 2)
#' z.3b <- setx(z.3, Petal.Length = 4.4)
#' z.3 <- sim(z.3, x = z.3a, x1 = z.3a)
#' zelig_qi_to_df(z.3)
#'
#' #### QIs for a range of fitted values
#' z.4 <- zelig(Petal.Width ~ Petal.Length + Species, data = iris,
#'              model = "ls")
#' z.4 <- setx(z.4, Petal.Length = 2:4)
#' z.4 <- sim(z.4)
#' zelig_qi_to_df(z.4)
#'
#' #### QIs for a range of fitted values, estimated by Species
#' z.5 <- zelig(Petal.Width ~ Petal.Length, by = "Species", data = iris,
#'             model = "ls")
#' z.5 <- setx(z.5, Petal.Length = 2:4)
#' z.5 <- sim(z.5)
#' zelig_qi_to_df(z.5)
#'
#' #### QIs for two ranges of fitted values
#' z.6 <- zelig(Petal.Width ~ Petal.Length + Species, data = iris,
#'             model = "ls")
#' z.6a <- setx(z.6, Petal.Length = 2:4, Species = 'setosa')
#' z.6b <- setx(z.6, Petal.Length = 2:4, Species = 'virginica')
#' z.6 <- sim(z.6, x = z.6a, x1 = z.6b)
#'
#' zelig_qi_to_df(z.6)
#'
#' @source For a discussion of tidy data see
#' <https://vita.had.co.nz/papers/tidy-data.pdf>.
#'
#'
#' @md
#' @author Christopher Gandrud
#' @export

zelig_qi_to_df <- function(obj) {
  message('zelig_qi_to_df is an experimental function.\n  Please report issues to: https://github.com/IQSS/Zelig/issues')
  
  is_zelig(obj)
  is_sims_present(obj$sim.out)
  
  comb <- data.frame()
  if (is_simsx(obj$sim.out, fail = FALSE)) {
    comb_temp <- extract_setx(obj)
    comb <- rbind(comb, comb_temp)
  }
  if (is_simsx1(obj$sim.out, fail = FALSE)) {
    comb_temp <- extract_setx(obj, which_x = 'x1')
    comb <- rbind(comb, comb_temp)
  }
  if (is_simsrange(obj$sim.out, fail = FALSE)) {
    comb_temp <- extract_setrange(obj)
    comb <- rbind(comb, comb_temp)
  }
  if (is_simsrange1(obj$sim.out, fail = FALSE)) {
    comb_temp <- extract_setrange(obj, which_range = 'range1')
    comb <- rbind(comb, comb_temp)
  }
  
  # Need range1
  if (nrow(comb) == 0) stop('Unable to find simulated quantities of interest.',
                            call. = FALSE)
  return(comb)
}


#' Extract setx for non-range and return tidy formatted data frame
#'
#' @param obj a zelig object containing simulated quantities of interest
#' @param which_x character string either `'x'` or `'x1'` indicating whether
#'   to extract the first or second set of fitted values
#'
#' @md
#' @internal

extract_setx <- function(obj, which_x = 'x') {
  
  temp_comb <- data.frame()
  all_fitted <- obj$setx.out[[which_x]]
  all_sims <- obj$sim.out[[which_x]]
  
  temp_fitted <- as.data.frame(all_fitted$mm[[1]],
                               row.names = NULL)
  
  by_length <- nrow(all_fitted)
  if (by_length > 1) {
    temp_fitted <- temp_fitted[rep(seq_len(nrow(temp_fitted)), by_length), ]
    temp_fitted <- data.frame(by = all_fitted[[1]],
                              temp_fitted, row.names = NULL)
  }
  temp_fitted <- rm_intercept(temp_fitted)
  
  temp_ev <- lapply(all_sims$ev, unlist)
  temp_pv <- lapply(all_sims$pv, unlist)
  for (i in 1:nrow(temp_fitted)) {
    temp_qi <- data.frame(temp_ev[[i]], temp_pv[[i]])
    names(temp_qi) <- c('expected_value', 'predicted_value')
    
    temp_df <- cbind(temp_fitted[i, ], temp_qi, row.names = NULL)
    temp_comb <- rbind(temp_comb, temp_df)
  }
  temp_comb$x <- which_x
  temp_comb <- temp_comb[, c(ncol(temp_comb), 1:(ncol(temp_comb)-1))]
  
  return(temp_comb)
}

#' Extract setrange fors return tidy formatted data frame
#'
#' @param obj a zelig object containing a range of simulated quantities of
#'   interest
#' @param which_range character string either `'range'` or `'range1'` 
#'   indicating whether to extract the first or second set of fitted values
#'
#' @md
#' @internal

extract_setrange <- function(obj, which_range = 'range') {
  
  temp_comb <- data.frame()
  all_fitted <- obj$setx.out[[which_range]]
  all_sims <- obj$sim.out[[which_range]]
  
  for (i in 1:length(all_fitted)) {
    temp_fitted <- as.data.frame(all_fitted[[i]]$mm[[1]], row.names = NULL)
    
    by_length <- nrow(all_fitted[[i]])
    if (by_length > 1) {
      temp_fitted <- temp_fitted[rep(seq_len(nrow(temp_fitted)), 
                                     by_length), ]
      temp_fitted <- data.frame(by = all_fitted[[i]][[1]], temp_fitted, 
                                row.names = NULL)
    }
    temp_fitted <- rm_intercept(temp_fitted)
    
    
    temp_ev <- lapply(all_sims[[i]]$ev, unlist)
    temp_pv <- lapply(all_sims[[i]]$pv, unlist)
    
    temp_comb_1_range <- data.frame()
    for (u in 1:nrow(temp_fitted)) {
      temp_qi <- data.frame(temp_ev[[u]], temp_pv[[u]])
      names(temp_qi) <- c('expected_value', 'predicted_value')
      temp_df <- cbind(temp_fitted[u, ], temp_qi, row.names = NULL)
      temp_comb_1_range <- rbind(temp_comb_1_range, temp_df)
    }
    temp_comb <- rbind(temp_comb, temp_comb_1_range)
  }
  if (which_range == 'range') temp_comb$x <- 'x'
  else temp_comb$x <- 'x1'
  temp_comb <- temp_comb[, c(ncol(temp_comb), 1:(ncol(temp_comb)-1))]
  
  return(temp_comb)
}
