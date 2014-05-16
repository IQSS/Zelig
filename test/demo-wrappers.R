source(file.path("..", "R", "utils.R"))
source(file.path("..", "R", "model-zelig.R"))
source(file.path("..", "R", "model-ls.R"))
source(file.path("..", "R", "model-glm.R"))
source(file.path("..", "R", "model-binchoice.R"))
source(file.path("..", "R", "model-logit.R"))
source(file.path("..", "R", "model-probit.R"))
source(file.path("..", "R", "model-poisson.R"))
source(file.path("..", "R", "model-normal.R"))
source(file.path("..", "R", "model-gamma.R"))
source(file.path("..", "R", "model-negbinom.R"))
source(file.path("..", "R", "model-exp.R"))
source(file.path("..", "R", "model-lognorm.R"))
source(file.path("..", "R", "model-tobit.R"))

source(file.path("..", "R", "wrappers.R"))

library(MASS)

z.out <- zeligw(dist ~ speed, model = "ls", data = cars)
print(z.out)
x.out <- setxw(z.out, speed=30)
x1.out <- setxw(z.out, speed=40)
s.out <- simw(z.out, x.out, x1.out, num = 1000)
summaryw(s.out)

f <- function(form = dist ~ speed, model = "ls", data = cars) {
  zz.out <- zeligw(formula = form, model = model, data = data)
  print(zz.out)
  xx.out <- setxw2(zz.out, speed=30)
  ss.out <- simw2(zz.out, xx.out, num = 1000)
  ss.out$summarize()
}

f()

FF <- function(form = dist ~ speed, data = cars) {
  Z <- zls$new()
  Z$zelig(formula = form, data = data)
  print(Z)
  Z$setx(speed = 3)
  return(Z)
}

ZZ <- FF()
