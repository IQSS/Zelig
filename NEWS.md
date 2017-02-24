Zelig version 5.0-16
==============================

## Major changes

- !! Breaking change !! the `get*` functions (e.g. `get_coef`) now use
underscores `_` to delimit words in the function names (e.g. `get_coef`). #214

- Added a number of new "getter" methods for extracting estimation elements:

    + `get_names` method to return Zelig object field names. Also available via
  `names`. #216

    + `get_residuals` to extract fitted model residuals. Also available via
  `residuals`.

    + `get_df_residuals` method to return residual degrees-of-freedom.
  Also accessible via `df.residuals`.

    + `get_model_data` method to return the data frame used to estimate the
  original model.

    + `get_pvalue` and `get_se` methods to return estimated model p-values and
  standard errors. #147

- `zelig_qi_to_df` function for extracting simulated quantities of interest
from a Zelig object and returning them as a tidy-formatted data frame.

- Extensive additions to model estimation method documentation and vignettes.

- `setx` returns an error if it is unable to find a supplied variable name. 

- `setx1` wrapper added to facilitate piped workflows for first differences.

- `zelig` can handle independent variables that are transformed using the 
natural logarithm inside of the call. #225

## Minor changes and bug fixes

- Internal code improvements.

- Corrected an issue where `plot` would tend to choose a factor level as the 
x-axis variable when plotting a range of simulations. #226

- If a factor level variable's fitted value is not specified in `setx` and
it is multi-modal, the last factor in the factor list is arbitarily chosen. 
This replaces previous behavior where the level was randomly chosen, causing
unuseful quantity of interest range plots. #226

- Corrected a bug where `summary` for ranges of `setx` would only show the 
first scenario. Now all scenarios are shown. #226

- Corrected a bug where the README.md was not included in the CRAN build.


Zelig version 5.0-15
==============================

## Major changes

- Allows users to convert an independent variable to a factor within a `zelig`
call using `as.factor`. #213

- `from_zelig_model` function to extract original fitted model objects from
`zelig` estimation calls. This is useful for conducting non-Zelig supported
post-estimation and easy integration with the texreg and stargazer packages
for formatted parameter estimate tables. #189

- Additional MC tests for a wide range of models. #160

## Minor changes

- Solved deep assignment issue that returned a series of warnings on build. #172

## Bug fixes

- Resolves a bug from `set` where `sim` would fail for models that included
factor level independent variables. #156

- Fixed an issue with `model-survey` where `ids` was hard coded as `~1`. #144

- Fixed `ATT` bug introduced in 5.0-14. #194

- Fixed `ci.plot` bug with `timeseries` models introduced in 5.0-15. #204


Zelig version 5.0-14
==============================

## Major changes

- `mode` has been deprecated. Please use `Mode`. #152

- The Zelig 4 `sim` wrapper now intelligently looks for fitted values from the
reference class object if not supplied via the \code{x} argument.

- New `to_zelig_mi` utility function for combining multiply imputed data sets 
for passing to `zelig`. `mi` will also work to enable backwards compatibility. 
#178

- Initial development on a new testing architecture and more tests for
`model-*`, Zelig 4 wrappers, `ci.plot`, and the Zelig workflow.

- `graph` method now accepts simulations from `setx` and `setrange`. For the
former it uses `qi.plot` and `ci.plot` for the latter.

- Improved error messages for Zelig 4 wrappers.

- Improved error messages if Zelig methods are supplied with too little
information.

- `model-arima` now fails if the dependent variable does not vary for one of the
cases.

## Minor changes

- Minor documentation improvements for Zelig 4 wrappers.

- Dynamically generated README.md.

- Removed plyr package dependency.

- `rbind_all` replaced by `bind_rows` as the former is deprecated by dplyr.

- Other internal code improvements