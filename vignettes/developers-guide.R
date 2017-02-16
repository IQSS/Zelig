## ----setup, include=FALSE------------------------------------------------
knitr::opts_knit$set(
  stop_on_error = 2L
)

## ----eval=FALSE----------------------------------------------------------
#  data(turnout)
#  z5 <- zlogit$new()
#  z5$zelig(vote ~ age, data = turnout)
#  z5$setx()
#  z5$sim()
#  z5$graph()

## ----eval=FALSE----------------------------------------------------------
#  data(turnout)
#  z5 <- zlogit$new()
#  z5$zelig(vote ∼ age, data = turnout)

## ---- eval=FALSE---------------------------------------------------------
#  fit.poisson <- glm(vote ∼ age, data=turnout, family = poisson())
#  fit.logit <- glm(vote ∼ age, data=turnout, family=binomial("logit"))
#  fit.probit <- glm(vote ∼ age, data=turnout, family=binomial("probit"))

## ----eval=FALSE----------------------------------------------------------
#  fit <- glm(vote ∼ age, data=turnout, family=binomial("logit"))

## ----eval=FALSE----------------------------------------------------------
#  z5 <- zlogit$new()
#  z5$zelig(vote ∼ age, data = turnout)

## ----eval=FALSE----------------------------------------------------------
#  z5 <- zelig(vote ∼ age, data = turnout, model = "logit")

## ----eval=FALSE----------------------------------------------------------
#  zlogit <- setRefClass("Zelig-logit",
#                        contains = "Zelig-binchoice")

## ---- eval=FALSE---------------------------------------------------------
#  zbinchoice <- setRefClass("Zelig-binchoice",
#                            contains = "Zelig-glm")

## ---- eval=FALSE---------------------------------------------------------
#  zglm <- setRefClass("Zelig-glm",
#                      contains = "Zelig",
#                      fields = list(family = "character",
#                                    link = "character",
#                                    linkinv = "function"))

## ---- eval=FALSE---------------------------------------------------------
#  zlogit$methods(initialize = function() {
#      callSuper()
#      .self$name <- "logit"
#      .self$link <- "logit"
#      .self$description = "Logistic Regression for Dichotomous Dependent Variables"
#      .self$packageauthors <- "R Core Team"
#      .self$wrapper <- "logit"
#  })

## ----eval=FALSE----------------------------------------------------------
#  .self$zelig.call <- match.call(expand.dots = TRUE)
#  .self$model.call <- .self$zelig.call

