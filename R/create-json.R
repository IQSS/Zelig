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
#' @include model-quantile.R
#' @include model-relogit.R
#' @include model-gee.R
#' @include model-binchoice-gee.R
#' @include model-logit-gee.R
#' @include model-probit-gee.R
#' @include model-gamma-gee.R
#' @include model-normal-gee.R
#' @include model-poisson-gee.R
#' @include model-bayes.R
#' @include model-factor-bayes.R
#' @include model-logit-bayes.R
#' @include model-mlogit-bayes.R
#' @include model-normal-bayes.R
#' @include model-oprobit-bayes.R
#' @include model-poisson-bayes.R
#' @include model-probit-bayes.R
#' @include model-tobit-bayes.R
#' @include model-weibull.R
#' @include model-timeseries.R
#' @include model-arima.R
#' @include model-ar.R
#' @include model-ma.R

#library(jsonlite)

createJSON <- function(){

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

  z5negbin <- znegbin$new()
  z5negbin$toJSON()

  z5exp <- zexp$new()
  z5exp$toJSON()

  z5lognorm <- zlognorm$new()
  z5lognorm$toJSON()

  z5tobit <- ztobit$new()
  z5tobit$toJSON()

  z5quantile <- zquantile$new()
  z5quantile$toJSON()

  z5relogit <- zrelogit$new()
  z5relogit$toJSON()

  z5logitgee <- zlogitgee$new()
  z5logitgee$toJSON()

  z5probitgee <- zprobitgee$new()
  z5probitgee$toJSON()

  z5gammagee <- zgammagee$new()
  z5gammagee$toJSON()

  z5normalgee <- znormalgee$new()
  z5normalgee$toJSON()

  z5poissongee <- zpoissongee$new()
  z5poissongee$toJSON()

  z5factorbayes <- zfactorbayes$new()
  z5factorbayes$toJSON()

  z5logitbayes <- zlogitbayes$new()
  z5logitbayes$toJSON()

  z5mlogitbayes <- zmlogitbayes$new()
  z5mlogitbayes$toJSON()

  z5normalbayes <- znormalbayes$new()
  z5normalbayes$toJSON()

  z5oprobitbayes <- zoprobitbayes$new()
  z5oprobitbayes$toJSON()

  z5poissonbayes <- zpoissonbayes$new()
  z5poissonbayes$toJSON()

  z5probitbayes <- zprobitbayes$new()
  z5probitbayes$toJSON()

  z5tobitbayes <- ztobitbayes$new()
  z5tobitbayes$toJSON()

  z5weibull <- zweibull$new()
  z5weibull$toJSON()

  z5logitsurvey <- zlogitsurvey$new()
  z5logitsurvey$toJSON()

  z5probitsurvey <- zprobitsurvey$new()
  z5probitsurvey$toJSON()

  z5gammasurvey <- zgammasurvey$new()
  z5gammasurvey$toJSON()

  z5normalsurvey <- znormalsurvey$new()
  z5normalsurvey$toJSON()

  z5poissonsurvey <- zpoissonsurvey$new()
  z5poissonsurvey$toJSON()
  
  z5arima <- zarima$new()
  z5arima$toJSON()

  z5ar <- zar$new()
  z5ar$toJSON()

  z5ma <- zma$new()
  z5ma$toJSON()

  zeligmodels <- list(zelig5models = list("ls" = z5ls$ljson,
                    "logit" = z5logit$ljson,
                    "probit" = z5probit$ljson,
                    "poisson" = z5poisson$ljson,
                    "normal" = z5normal$ljson,
                    "gamma" = z5gamma$ljson,
                    "negbin" = z5negbin$ljson,
                    "exp" = z5exp$ljson,
                    "lognorm" = z5lognorm$ljson,
                    "tobit" = z5tobit$ljson,
                    "quantile" = z5quantile$ljson,
                    "relogit" = z5relogit$ljson,
                    "logitgee" = z5logitgee$ljson,
                    "probitgee" = z5probitgee$ljson,
                    "gammagee" = z5gammagee$ljson,
                    "normalgee" = z5normalgee$ljson,
                    "poissongee" = z5poissongee$ljson,
                    "factorbayes" = z5factorbayes$ljson,
                    "logitbayes" = z5logitbayes$ljson,
                    "mlogitbayes" = z5mlogitbayes$ljson,
                    "normalbayes" = z5normalbayes$ljson,
                    "oprobitbayes" = z5oprobitbayes$ljson,
                    "poissonbayes" = z5poissonbayes$ljson,
                    "probitbayes" = z5probitbayes$ljson,
                    "tobitbayes" = z5tobitbayes$ljson,
                    "weibull" = z5weibull$ljson,
                    "logitsurvey" = z5logitsurvey$ljson,
                    "probitsurvey" = z5probitsurvey$ljson,
                    "normalsurvey" = z5normalsurvey$ljson,
                    "gammasurvey" = z5gammasurvey$ljson,
                    "poissonsurvey" = z5poissonsurvey$ljson,
                    "arima" = z5arima$ljson, 
                    "ma" = z5ma$ljson,
                    "ar" = z5ar$ljson))

  # cat(toJSON(zeligmodels, pretty = TRUE), file = file.path("tools", "zelig5models.json"))
  # file.copy(from = file.path("tools", "zelig5models.json"), to = file.path("inst", "JSON", "zelig5models.json"))

  cat(toJSON(zeligmodels, pretty = TRUE), "\n", file = file.path("zelig5models.json"))
  file.rename(from = file.path("zelig5models.json"), to = file.path("inst", "JSON", "zelig5models.json"))
  file.remove(file.path("zelig5models.json"))

  # cat(toJSON(zeligmodels, pretty = TRUE))
  # j <- jsonlite::fromJSON(txt = readLines(file.path("..", "/JSON", "/zelig5models.json")))
  
  return(TRUE)
}
