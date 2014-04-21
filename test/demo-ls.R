library(MASS)
data(cars)

source(file.path("..", "R", "utils.R"))
source(file.path("..", "R", "model-zelig.R"))
source(file.path("..", "R", "model-ls.R"))

# Zelig 4 code:
library(Zelig)
z.out <- zelig(dist ~ speed, cars, model="ls")
summary(z.out)
x.out <- setx(z.out, speed=30)
set.seed(42)
s.out <- sim(z.out, x.out)
summary(s.out)

# Zelig 5 code:
z5 <- zls$new()
z5$zelig(dist ~ speed, data=cars)
z5
z5$model.call
z5$zelig.call
z5$setx(speed=30)
set.seed(42)
z5$sim(num=100)
z5$summarize()
z5$cite()
