#' @include utils.R
#' @include model-zelig.R
#' @include model-ls.R
#' @include model-glm.R
#' @include model-binchoice.R
#' @include model-logit.R
#' @include model-probit.R
#' @include model-poisson.R
#' @include model-normal.R
#' @include model-gamma.R
#' @include model-negbinom.R
#' @include model-exp.R
#' @include model-lognorm.R
#' @include model-tobit.R

library(survival)
library(jsonlite)

z5ls <- zls$new()
z5ls$toJSON()

z5logit <- zlogit$new()
z5logit$toJSON()

z5probit <- zprobit$new()
z5probit$toJSON()

z5poisson <- zpoisson$new()
z5poisson$toJSON()

z5normal <- znormal$new()
z5normal$toJSON()

z5gamma <- zgamma$new()
z5gamma$toJSON()

z5negbinom <- znegbinom$new()
z5negbinom$toJSON()

z5negbinom <- znegbinom$new()
z5negbinom$toJSON()

z5negbinom <- znegbinom$new()
z5negbinom$toJSON()

z5exp <- zexp$new()
z5exp$toJSON()

z5lognorm <- zlognorm$new()
z5lognorm$toJSON()

z5tobit <- ztobit$new()
z5tobit$toJSON()

zeligmodels <- list(zelig5models = list("ls" = z5ls$ljson,
                    "logit" = z5logit$ljson,
                    "probit" = z5probit$ljson,
                    "poisson" = z5poisson$ljson,
                    "normal" = z5normal$ljson,
                    "gamma" = z5gamma$ljson,
                    "negbinom" = z5negbinom$ljson,
                    "exp" = z5exp$ljson,
                    "lognorm" = z5lognorm$ljson,
                    "tobit" = z5tobit$ljson))

# cat(toJSON(zeligmodels, pretty = TRUE))

# cat(toJSON(zeligmodels, pretty = TRUE),
#     file = file.path("JSON", "zelig5models.json"))

# j <- fromJSON(txt = file.path("JSON", "zelig5models.json"))
