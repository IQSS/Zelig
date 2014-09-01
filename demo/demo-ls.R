library(MASS)
data(cars)

# Zelig 4 code:
library(Zelig4)
z.out <- zelig(dist ~ speed, cars, model = "ls")
summary(z.out)
x.out <- setx(z.out, speed = 30)
set.seed(42)
s.out <- sim(z.out, x.out, num = 100)
summary(s.out)

# Zelig 5 code:
z5 <- zls$new()
z5$zelig(dist ~ speed, data = cars)
z5
z5$zelig.out
z5$model.call
z5$zelig.call
z5$setx(sascxasx = 9879, speed = 30, sdjchbsdc = 87, kcsbc = 8787)
z5$setx(sascxasx = 9879)
z5$setx1(speed = 30)
z5$setx()
z5$setx.out
z5
set.seed(42)
z5$sim(num=3)
z5$sim.out
z5
z5$summarize()
z5$cite()

z5$setrange(speed = 30:32,speed = 19:23)
z5$setx.out
set.seed(42)
z5$sim(num=3)
z5$sim.out
z5$summarize()
z5
