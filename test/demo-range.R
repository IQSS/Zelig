library(Zelig)

data(turnout)
z.out <- zelig(vote ~ race + educate + age + I(age^2) + income,
               model = "ls", data = turnout)
summary(z.out)
x.low <- setx(z.out, educate = 12:13, age = 18:20, race = c("white", "black"))
x.low
x.high <- setx(z.out, educate = 12:13, age = 18:20, race = c("white", "black"))
x.high
set.seed(42)
s.out <- sim(z.out, x = x.low, x1 = x.high, num = 5)
summary(s.out)

source(file.path("..", "R", "utils.R"))
source(file.path("..", "R", "model-zelig.R"))
source(file.path("..", "R", "model-ls.R"))
source(file.path("..", "R", "model-glm.R"))
source(file.path("..", "R", "model-binchoice.R"))
source(file.path("..", "R", "model-logit.R"))

## Zelig 5
z5 <- zls$new()
z5$zelig(vote ~ race + educate + age + I(age^2) + income, data = turnout)
z5
z5$setx(educate = 12, age = 18)
z5$setx1(educate = 12, age = 20)
z5$setrange(educate = 12:13, age = 18:20)

z5$setx.out$x <- NULL
z5$setx.out$x1 <- NULL

z5$setx.out$range

z5$setx.out

set.seed(42)
z5$sim(num = 5)

z5$ev("range")
z5$pv("range")

z5$summarize()



