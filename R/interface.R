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
#' @details A simulated quantities of interest in a tidy data formatted
#'   `data.frame`. This can be useful for creating custom plots.
#'
#'  Each row contains a simulated value and each column contains:
#'
#'  - `setx_value` whether the simulations are from the base `x` `setx` or the
#'      contrasting `x1` for finding first differences.
#'  - The fitted values specified in `setx` including a `by` column if
#'     `by` was used in the \code{\link{zelig}} call.
#'  - `expected_value`
#'  - `predicted_value`
#'
#' @examples
#' #### QIs without first difference or range, from covariates fitted at
#' ## central tendencies
#' z.1 <- zelig(Petal.Width ~ Petal.Length + Species, data = iris,
#'              model = "ls")
#' z.1 <- setx(z.1)
#' z.1 <- sim(z.1)
#' head(zelig_qi_to_df(z.1))
#'
#' #### QIs for first differences
#' z.2 <- zelig(Petal.Width ~ Petal.Length + Species, data = iris,
#'              model = "ls")
#' z.2a <- setx(z.2, Petal.Length = 2)
#' z.2b <- setx(z.2, Petal.Length = 4.4)
#' z.2 <- sim(z.2, x = z.2a, x1 = z.2a)
#' head(zelig_qi_to_df(z.2))
#'
#' #### QIs for first differences, estimated by Species
#' z.3 <- zelig(Petal.Width ~ Petal.Length, by = "Species", data = iris,
#'              model = "ls")
#' z.3a <- setx(z.3, Petal.Length = 2)
#' z.3b <- setx(z.3, Petal.Length = 4.4)
#' z.3 <- sim(z.3, x = z.3a, x1 = z.3a)
#' head(zelig_qi_to_df(z.3))
#'
#' #### QIs for a range of fitted values
#' z.4 <- zelig(Petal.Width ~ Petal.Length + Species, data = iris,
#'              model = "ls")
#' z.4 <- setx(z.4, Petal.Length = 2:4)
#' z.4 <- sim(z.4)
#' head(zelig_qi_to_df(z.4))
#'
#' #### QIs for a range of fitted values, estimated by Species
#' z.5 <- zelig(Petal.Width ~ Petal.Length, by = "Species", data = iris,
#'             model = "ls")
#' z.5 <- setx(z.5, Petal.Length = 2:4)
#' z.5 <- sim(z.5)
#' head(zelig_qi_to_df(z.5))
#'
#' #### QIs for two ranges of fitted values
#' z.6 <- zelig(Petal.Width ~ Petal.Length + Species, data = iris,
#'             model = "ls")
#' z.6a <- setx(z.6, Petal.Length = 2:4, Species = "setosa")
#' z.6b <- setx(z.6, Petal.Length = 2:4, Species = "virginica")
#' z.6 <- sim(z.6, x = z.6a, x1 = z.6b)
#'
#' head(zelig_qi_to_df(z.6))
#'
#' @source For a discussion of tidy data see
#' <https://www.jstatsoft.org/article/view/v059i10>.
#'
#' @seealso \code{\link{qi_slimmer}}
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

#' Extracted fitted values from a Zelig object with `setx` values
#'
#' @param obj a zelig object with simulated quantities of interest
#'
#' @details Fitted (`setx`) values in a tidy data formatted
#'   `data.frame`. This was designed to enable the WhatIf package's
#'   `whatif` function to extract "counterfactuals".
#'
#' @examples
#' #### QIs without first difference or range, from covariates fitted at
#' ## central tendencies
#' z.1 <- zelig(Petal.Width ~ Petal.Length + Species, data = iris,
#'              model = "ls")
#' z.1 <- setx(z.1)
#' zelig_setx_to_df(z.1)
#'
#' #### QIs for first differences
#' z.2 <- zelig(Petal.Width ~ Petal.Length + Species, data = iris,
#'              model = "ls")
#' z.2 <- setx(z.2, Petal.Length = 2)
#' z.2 <- setx1(z.2, Petal.Length = 4.4)
#' zelig_setx_to_df(z.2)
#'
#' #### QIs for first differences, estimated by Species
#' z.3 <- zelig(Petal.Width ~ Petal.Length, by = "Species", data = iris,
#'              model = "ls")
#' z.3 <- setx(z.3, Petal.Length = 2)
#' z.3 <- setx1(z.3, Petal.Length = 4.4)
#' zelig_setx_to_df(z.3)
#'
#' #### QIs for a range of fitted values
#' z.4 <- zelig(Petal.Width ~ Petal.Length + Species, data = iris,
#'              model = "ls")
#' z.4 <- setx(z.4, Petal.Length = 2:4)
#' zelig_setx_to_df(z.4)
#'
#' #### QIs for a range of fitted values, estimated by Species
#' z.5 <- zelig(Petal.Width ~ Petal.Length, by = "Species", data = iris,
#'              model = "ls")
#' z.5 <- setx(z.5, Petal.Length = 2:4)
#' zelig_setx_to_df(z.5)
#'
#' #### QIs for two ranges of fitted values
#' z.6 <- zelig(Petal.Width ~ Petal.Length + Species, data = iris,
#'              model = "ls")
#' z.6 <- setx(z.6, Petal.Length = 2:4, Species = "setosa")
#' z.6 <- setx1(z.6, Petal.Length = 2:4, Species = "virginica")
#' zelig_setx_to_df(z.6)
#'
#' @md
#' @author Christopher Gandrud
#' @export

zelig_setx_to_df <- function(obj) {

    is_zelig(obj)

    comb <- data.frame()
    if (!is.null(obj$setx.out$x)) {
        comb_temp <- extract_setx(obj, only_setx = TRUE)
        comb <- rbind(comb, comb_temp)
    }
    if (!is.null(obj$setx.out$x1)) {
        comb_temp <- extract_setx(obj, which_x = 'x1', only_setx = TRUE)
        comb <- rbind(comb, comb_temp)
    }
    if (!is.null(obj$setx.out$range)) {
        comb_temp <- extract_setrange(obj, only_setx = TRUE)
        comb <- rbind(comb, comb_temp)
    }
    if (!is.null(obj$setx.out$range1)) {
        comb_temp <- extract_setrange(obj, which_range = 'range1',
                                      only_setx = TRUE)
        comb <- rbind(comb, comb_temp)
    }

    # Need range1
    if (nrow(comb) == 0) stop('Unable to find fitted (setx) values.',
                              call. = FALSE)
    return(comb)
}


#' Extract setx for non-range and return tidy formatted data frame
#'
#' @param obj a zelig object containing simulated quantities of interest
#' @param which_x character string either `'x'` or `'x1'` indicating whether
#'   to extract the first or second set of fitted values
#' @param only_setx logical whether or not to only extract `setx`` values.
#'
#' @seealso \code{\link{zelig_qi_to_df}}
#' @author Christopher Gandrud
#'
#' @md
#' @internal

extract_setx <- function(obj, which_x = 'x', only_setx = FALSE) {

    temp_comb <- data.frame()
    all_fitted <- obj$setx.out[[which_x]]
    if (!only_setx) all_sims <- obj$sim.out[[which_x]]

    temp_fitted <- as.data.frame(all_fitted$mm[[1]],
                                row.names = NULL)

    by_length <- nrow(all_fitted)
    if (by_length > 1) {
        temp_fitted <- temp_fitted[rep(seq_len(nrow(temp_fitted)), by_length), ]
        temp_fitted <- data.frame(by = all_fitted[[1]],
                                    temp_fitted, row.names = NULL)
    }
    temp_fitted <- rm_intercept(temp_fitted)
    temp_fitted <- factor_coef_combine(obj, temp_fitted)

    if (!only_setx) {
        temp_ev <- lapply(all_sims$ev, unlist)
         temp_pv <- lapply(all_sims$pv, unlist)
         for (i in 1:nrow(temp_fitted)) {
            temp_qi <- data.frame(temp_ev[[i]], temp_pv[[i]])
            names(temp_qi) <- c('expected_value', 'predicted_value')

            temp_df <- cbind(temp_fitted[i, ], temp_qi, row.names = NULL)
            temp_comb <- rbind(temp_comb, temp_df)
        }
        temp_comb$setx_value <- which_x
        temp_comb <- temp_comb[, c(ncol(temp_comb), 1:(ncol(temp_comb)-1))]

        return(temp_comb)
    }
    else if (only_setx) return(temp_fitted)

}

#' Extract setrange to return as tidy formatted data frame
#'
#' @param obj a zelig object containing a range of simulated quantities of
#'   interest
#' @param which_range character string either `'range'` or `'range1'`
#'   indicating whether to extract the first or second set of fitted values
#' @param only_setx logical whether or not to only extract `setx`` values.
#'
#' @seealso \code{\link{zelig_qi_to_df}}
#' @author Christopher Gandrud
#'
#' @md
#' @internal

extract_setrange <- function(obj, which_range = 'range', only_setx = FALSE) {

    temp_comb <- data.frame()
    all_fitted <- obj$setx.out[[which_range]]
    if (!only_setx) all_sims <- obj$sim.out[[which_range]]

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
        temp_fitted <- factor_coef_combine(obj, temp_fitted)

        if (!only_setx) {
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
        else if (only_setx) {
            temp_comb <- rbind(temp_comb, temp_fitted)
        }
    }
    if (!only_setx) {
        if (which_range == 'range') temp_comb$setx_value <- 'x'
        else temp_comb$setx_value <- 'x1'
        temp_comb <- temp_comb[, c(ncol(temp_comb), 1:(ncol(temp_comb)-1))]
    }
    return(temp_comb)
}

#' Return individual factor coefficient fitted values to single factor variable
#'
#' @param obj a zelig object with an estimated model
#' @param fitted a data frame with values fitted by \code{setx}. Note
#' created internally by \code{\link{extract_setx}} and
#'   \code{\link{extract_setrange}}
#'
#' @author Christopher Gandrud
#' @internal

factor_coef_combine <- function(obj, fitted) {

    is_zelig(obj)

    original_data <- obj$zelig.out$z.out[[1]]$model
    factor_vars <- sapply(original_data, is.factor)
    if (any(factor_vars)) {
        for (i in names(original_data)[factor_vars]) {
            if (!(i %in% names(fitted))) {
                matches_name <- names(fitted)[grepl(sprintf('^%s*', i),
                                                    names(fitted))]
                var_levels <- levels(original_data[, i])
                fitted[, i] <- NA
                for (u in matches_name) {
                    label_value <- gsub(sprintf('^%s', i), '', u)
                    fitted[, i][fitted[, u] == 1] <- label_value
                }
                ref_level <- var_levels[!(var_levels %in%
                                              gsub(sprintf('^%s', i), '',
                                                   matches_name))]
                fitted[, i][is.na(fitted[, i])] <- ref_level
                fitted[, i] <- factor(fitted[, i], levels = var_levels)
                fitted <- fitted[, !(names(fitted) %in% matches_name)]
            }
        }
    }
    return(fitted)
}


#' Find the median and a central interval of simulated quantity of interest
#' distributions
#'
#' @param df a tidy-formatted data frame of simulated quantities of interest
#'   created by \code{\link{zelig_qi_to_df}}.
#' @param qi_type character string either `ev` or `pv` for returning the
#'   central intervals for the expected value or predicted value, respectively.
#' @param ci numeric. The central interval to return, expressed on the
#' `(0, 100]` or the equivalent `(0, 1]` interval.
#'
#' @details A tidy-formatted data frame with the following columns:
#'
#'   - The values fitted with \code{\link{setx}
#'   - `qi_ci_min`: the minimum value of the central interval specified with
#'   `ci`
#'   - `qi_ci_median`: the median of the simulated quantity of interest
#'   distribution
#'   - `qi_ci_max`: the maximum value of the central interval specified with
#'   `ci`
#'
#' @examples
#' library(dplyr)
#' qi.central.interval <- zelig(Petal.Width ~ Petal.Length + Species,
#'              data = iris, model = "ls") %>%
#'              setx(Petal.Length = 2:4, Species = "setosa") %>%
#'              sim() %>%
#'              zelig_qi_to_df() %>%
#'              qi_slimmer()
#'
#' @importFrom dplyr bind_rows %>%
#' @seealso \code{\link{zelig_qi_to_df}}
#' @author Christopher Gandrud
#' @md

qi_slimmer <- function(df, qi_type = 'ev', ci = 0.95) {
    qi__ <- scenario__ <- NULL

    if (!is.data.frame(df))
        stop('df must be a data frame created by zelig_qi_to_df.', call. = FALSE)
    if (!all(c('expected_value', 'predicted_value') %in% names(df)))
        stop('The data frame does not appear to have been created by zelig_qi_to_df.',
             call. = FALSE)

    ci <- ci_check(ci)
    lower <- (1 - ci)/2
    upper <- 1 - lower

    if (!(qi_type %in% c('ev', 'pv')))
        stop('qi_type must be either "ev" or "pv". ', call. = FALSE)
    if (qi_type == 'ev') qi_drop <- 'predicted_value'
    else qi_drop <- 'expected_value'

    if (qi_type == 'ev') qi_var <- 'expected_value'
    else qi_var <- 'predicted_value'

    if (qi_type == 'ev') qi_msg <- 'Expected Values'
    else qi_msg <- 'Predicted Values'
    message(sprintf('Slimming %s . . .', qi_msg))

    names_df <- names(df)
    df <- df[, !(names(df) %in% qi_drop)] # drop non-requested qi_type

    names(df)[names(df) == qi_var] <- 'qi__'
    df$scenario__ <- interaction(df[, !(names(df) %in% 'qi__')], drop = TRUE)

    qi_list <- split(df, df[['scenario__']])
    qi_list <- lapply(seq_along(qi_list), function(x){
        lower_bound <- quantile(qi_list[[x]][, 'qi__'], prob = lower)
        upper_bound <- quantile(qi_list[[x]][, 'qi__'], prob = upper)
        subset(qi_list[[x]], qi__ >= lower_bound & qi__ <= upper_bound)
    })

    df_slimmed <- data.frame(bind_rows(qi_list))

    df_out <- df_slimmed %>% group_by(scenario__) %>%
        summarise(qi_ci_min = min(qi__),
                  qi_ci_median = median(qi__),
                  qi_ci_max = max(qi__)
        ) %>%
        data.frame

    scenarios_df <- df[!duplicated(df$scenario__), !(names(df) %in% 'qi__')] %>%
                      data.frame(row.names = NULL)
    df_out <- merge(scenarios_df, df_out, by = 'scenario__', sort = FALSE)
    df_out$scenario__ <- NULL

    return(df_out)
}

#' Convert \code{ci} interval from percent to proportion and check if valid
#' @param x numeric. The central interval to return, expressed on the `(0, 100]`
#' or the equivalent `(0, 1]` interval.
#'
#' @md
#' @internal

ci_check <- function(x) {
    if (x > 1 & x <= 100) x <- x / 100
    if (x <= 0 | x > 1) {
        stop(sprintf("%s will not produce a valid central interval.", x),
              call. = FALSE)
    }
    return(x)
}
