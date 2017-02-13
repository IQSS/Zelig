Zelig version 5.0-16
==============================

## Minor changes

- Zelig 5 Quickstart vignette added.

- Added `names` method to return Zelig object field names. #216 

- Internal code improvements. 

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

- New `to_zelig_mi` utility function for combining multiply imputed datasets for
passing to `zelig`. `mi` will also work to enable backwards compatibility. #178

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
