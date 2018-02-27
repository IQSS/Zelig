> All changes to Zelig are documented here. GitHub issue numbers are given after
each change note when relevant. See <https://github.com/IQSS/Zelig/issues>.
External contributors are referenced with their GitHub usernames when
applicable.

Zelig version 5.1.6
==============================

## Major changes

- All Zelig time series models are deprecated.

## Minor changes

- `predit`, `fitted`, `residuals` now accept arguments. #320

Zelig version 5.1.5
==============================

++++ All Zelig time series models will be deprecated on 1 February 2018 ++++


## Bug fixes

-   Resolved an issue where `odds_ratios` standard errors were not correctly
returned for `logit` and `relogit` models. Thanks to @retrography. #302

-   Zelig 4 compatability wrappers now work for `arima` models. Thanks to
@mbsabath. #280

-   Resolved an error when only `setx` was called with `arima` models Thanks to
@mbsabath. #299

-   Resolved an error when `summary` was called after `sim` for `arima` models.
#305

-   Resolved an error when `sim` is used with differenced first-order
autoregressive models. #307

-   `arima` models return informative error when `data` is not found. #308

## Minor

-   Compatibility with testthat 2.0.0

-   Documentation updated to correctly reflect that `tobit` wraps `AER::tobit`.
#315

-   Package terminology in documentation corrected. #316

Zelig version 5.1-4
==============================

## Major changes

-   Speed improvements made to `relogit`. Thanks to @retrography. #88

-   Returns `relogit` weighted case control method to that described in
King and Langche (2001, eq. 11) and used in the Stata `relogit` implementation.
#295

-   Odds ratios now returned from `summary` with `relogit` models via the
`odds_ratios = TRUE` argument. #302

Zelig version 5.1-3
==============================

## Major changes

-   Roxygen documentation improvements.

## Minor changes and bug fixes

-   `zquantile` with Amelia imputed data now working. #277

-   `vcov` now works with `rq` quantile regression models.

-   More informative error handling for conflicting `timeseries` model
arguments. #283

-   Resolved and issue with `relogit` that produced a warning when the fitted
model object was passed to `predict`. #291


Zelig version 5.1-2
==============================

## Major changes

-   !EXPERIMENTAL! interface function `to_zelig` allows users to convert fitted model
objects fitted outside of Zelig to a Zelig object. The function is called
within the `setx` wrapper if a non-Zelig object is supplied. Currently
only works for models fitted with `lm` and many estimated with `glm` and
`svyglm`. #189

-   `get_se` and `get_pvalue` function wrappers created for `get_se` and
`get_pvalue` methods, respectively. #269

-   If `combine_coef_se` is given a model estimated without multiply imputed
data or bootstraps, an error is no longer returned. Instead a list of the
models' untransformed coefficients, standard errors, and p-values is returned. #268

-   `summary` for `logit` models now accepts the argument `odds_ratios`. When
`TRUE` odds ratio estimates are returned rather than coefficient estimates.
Thanks to Adam Obeng. PR/#270.

- `setx` and `sim` fail informatively when passed ZeligEI objects. #271

## Minor changes and bug fixes

-   Resolved a bug where `weights` were not being passed to `svydesign`
in survey models. #258

-   Due to limited functionality and instability, zelig survey estimations
no return a warning and a link to documentation on how to use `to_survey`
via `setx` to bipass `zelig`. #273

-   Resolved a bug where `from_zelig_model` would not extract fitted model
objects for models estimated using `vglm`. #265

-   `get_pvalue` and `get_se` now work for models estimated using `vglm`. #267

-   Improved `ivreg`, `mlogit`, and getter (#266) documentation.

Zelig version 5.1-1
==============================

## Minor changes

-   Average Treatment Effect on the Treated (ATT) vignette added to the online
documentation <http://docs.zeligproject.org/articles/att.html>

-   Corrected vignette URLs.


Zelig version 5.1-0
==============================

## Major changes

-   Introduce a new model type for instrumental-variable regression: `ivreg`
based on the `ivreg` from the AER package. #223

-   Use the Formula package for formulas. This will enable a common syntax for
multiple equations, though currently in Core Zelig it is only
enhances `ivreg`. #241

-   `zelig` calls now support `update`ing formulas (#244) and `.` syntax for
inserting all variables from `data` on the right-hand side of the formula
#87. See also #247.

-   Arbitrary `log` transformations are now supported in `zelig` calls
(exept for `ivreg` regressors). #225

-   Arbitrary `as.factor` and `factor` transformations are now supported in
`zelig` calls.

-   Restored quantile regression (`model = "rq"`). Currently only supports one
`tau` at a time. #255

-   Added `get_qi` wrapper for `get_qi` method.

-   Added `ATT` wrapper for `ATT` method.

-   `gee` models can now be estimated with multiply imputed data. #263

## Minor changes and bug fixes

-   `zelig` returns an error if `weights` are specified in a model estimated
with multiply imputed data. (not possible before, but uninformative error
returned)

-   Code improvement to `factor_coef_combine` so it does not return a warning
for model types with more than 1 declared class.

-   Reorganize README files to meet new CRAN requirements.

-   Switch `bind_rows` for `rbind_all` in `zquantile` as the latter is depricated.
#255

-   Depends on the survival package in order to enable `setx` for exponential
models without explicitly loading survival. #254

-   `relogit` now only accepts one `tau` per call (similar to `quantile`). Fixed
to address #257.

- Additional unit tests.

Zelig version 5.0-17
==============================

## Major changes

-   New function `combine_coef_se` takes as input a `zelig` model estimated
using multiply imputed data or bootstrapping and returns a list of coefficients,
standard errors, z-values, and p-values combined across the estimations. Thanks
to @vincentarelbundock for prompting. #229

-   The following changes were primarily made to re-established Zelig integration
with [WhatIf](https://CRAN.R-project.org/package=WhatIf). #236

    + Added `zelig_setx_to_df` for extracted fitted values created by `setx`.

    + Fitted factor level variable values are returned in a single column (not
by parameter level) by `zelig_qi_to_df`.

-   `setrange` (including `setx` used with a range of fitted values) now creates
scenarios based on matches of equal length set ranges. This enables `setx` to
work with polynomials, splines, etc. (currently only when these are created
outside of the `zelig` call). #238

## Minor changes and bug fixes

-   Resolve a bug where appropriate `plot`s were not created for `mlogitbayes`. #206

-   Arguments (such as `xlab`) can now be passed to `plot`. #237

-   `zelig_qi_to_df` and `qi_slimmer` bug with multinomial response models
resolved. #235

-   Resolved a bug where `coef`, `coefficients`, `vcov`, `fitted`, and `predict`
returned errors. Thanks to @vincentarelbundock for initially reporting. #231

-   Reduced number of digits show from `summary` for fitted model objects.



Zelig version 5.0-16
==============================

## Major changes

-   !! Breaking change !! the `get*` functions (e.g. `getcoef`) now use
underscores `_` to delimit words in the function names (e.g. `get_coef`). #214

-   Added a number of new "getter" methods for extracting estimation elements:

    + `get_names` method to return Zelig object field names. Also available via
  `names`. #216

    + `get_residuals` to extract fitted model residuals. Also available via
  `residuals`.

    + `get_df_residuals` method to return residual degrees-of-freedom.
  Also accessible via `df.residuals`.

    + `get_model_data` method to return the data frame used to estimate the
  original model.

    + `get_pvalue` and `get_se` methods to return estimated model p-values and
  standard errors. Thank you to @vincentarelbundock for contributions. #147

-   `zelig_qi_to_df` function for extracting simulated quantities of interest
from a Zelig object and returning them as a tidy-formatted data frame. #189

-   `setx` returns an error if it is unable to find a supplied variable name.

-   `setx1` wrapper added to facilitate piped workflows for first differences.

-   `zelig` can handle independent variables that are transformed using the
natural logarithm inside of the call. #225

## Minor changes and bug fixes

-   Corrected an issue where `plot` would tend to choose a factor level as the
x-axis variable when plotting a range of simulations. #226

-   If a factor level variable's fitted value is not specified in `setx` and
it is multi-modal, the last factor in the factor list is arbitrarily chosen.
This replaces previous behavior where the level was randomly chosen, causing
unuseful quantity of interest range plots. #226

-   Corrected a bug where `summary` for ranges of `setx` would only show the
first scenario. Now all scenarios are shown. #226

-   Corrected a bug where the README.md was not included in the CRAN build.

-   `to_zelig_mi` now can accept a list of data frames. Thanks to
@vincentarelbundock.

-   Internal code improvements.


Zelig version 5.0-15
==============================

## Major changes

-   Allows users to convert an independent variable to a factor within a `zelig`
call using `as.factor`. #213

-   `from_zelig_model` function to extract original fitted model objects from
`zelig` estimation calls. This is useful for conducting non-Zelig supported
post-estimation and easy integration with the texreg and stargazer packages
for formatted parameter estimate tables. #189

-   Additional MC tests for a wide range of models. #160

## Minor changes

-   Solved deep assignment issue that returned a series of warnings on build. #172

## Bug fixes

-   Resolves a bug from `set` where `sim` would fail for models that included
factor level independent variables. #156

-   Fixed an issue with `model-survey` where `ids` was hard coded as `~1`. #144

-   Fixed `ATT` bug introduced in 5.0-14. #194

-   Fixed `ci.plot` bug with `timeseries` models introduced in 5.0-15. #204


Zelig version 5.0-14
==============================

## Major changes

-   `mode` has been deprecated. Please use `Mode`. #152

-   The Zelig 4 `sim` wrapper now intelligently looks for fitted values from the
reference class object if not supplied via the x argument.

-   New `to_zelig_mi` utility function for combining multiply imputed data sets
for passing to `zelig`. `mi` will also work to enable backwards compatibility. #178

-   Initial development on a new testing architecture and more tests for
`model-*`, Zelig 4 wrappers, `ci.plot`, and the Zelig workflow.

-   `graph` method now accepts simulations from `setx` and `setrange`. For the
former it uses `qi.plot` and `ci.plot` for the latter.

-   Improved error messages for Zelig 4 wrappers.

-   Improved error messages if Zelig methods are supplied with too little
information.

-   `model-arima` now fails if the dependent variable does not vary for one of the
cases.

## Minor changes

-   Minor documentation improvements for Zelig 4 wrappers.

-   Dynamically generated README.md.

-   Removed plyr package dependency.

-   `rbind_all` replaced by `bind_rows` as the former is deprecated by dplyr.

-   Other internal code improvements
