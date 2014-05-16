source("utils.R")
source("model-zelig.R")
source("model-ls.R")
source("model-glm.R")
source("model-binchoice.R")
source("model-logit.R")
source("model-probit.R")
source("model-poisson.R")
source("model-normal.R")
source("model-gamma.R")
source("model-negbinom.R")
source("model-exp.R")
source("model-lognorm.R")
source("model-tobit.R")

library(survival)
library(jsonlite)

z5ls <- zls$new()
cat(z5ls$toJSON())

z5logit <- zlogit$new()
cat(z5logit$toJSON())

z5probit <- zprobit$new()
cat(z5probit$toJSON())

z5poisson <- zpoisson$new()
cat(z5poisson$toJSON())

z5normal <- znormal$new()
cat(z5normal$toJSON())

z5gamma <- zgamma$new()
cat(z5gamma$toJSON())

z5negbinom <- znegbinom$new()
cat(z5negbinom$toJSON())

z5negbinom <- znegbinom$new()
cat(z5negbinom$toJSON())

z5negbinom <- znegbinom$new()
cat(z5negbinom$toJSON())

z5exp <- zexp$new()
cat(z5exp$toJSON())

z5lognorm <- zlognorm$new()
cat(z5lognorm$toJSON())

z5tobit <- ztobit$new()
cat(z5tobit$toJSON())

zeligmodels <- list(zeligmodels = list("ls" = z5ls$ljson,
                    "logit" = z5logit$ljson,
                    "probit" = z5probit$ljson,
                    "poisson" = z5poisson$ljson,
                    "normal" = z5normal$ljson,
                    "gamma" = z5gamma$ljson,
                    "negbinom" = z5negbinom$ljson,
                    "exp" = z5exp$ljson,
                    "lognorm" = z5lognorm$ljson,
                    "tobit" = z5tobit$ljson))

# zeligmodels <- list(zeligmodels = list(z5ls$ljson,
#                                        z5logit$ljson,
#                                        z5probit$ljson,
#                                        z5poisson$ljson,
#                                        z5normal$ljson,
#                                        z5gamma$ljson,
#                                        z5negbinom$ljson,
#                                        z5exp$ljson,
#                                        z5lognorm$ljson,
#                                        z5tobit$ljson))

cat(toJSON(zeligmodels, pretty = TRUE))

cat(toJSON(zeligmodels, pretty = TRUE), file = "zelig5modelsnames.json")

j <- fromJSON(txt="zelig5modelsnames.json")
