library(MASS)

# Zelig 4 code:
library(Zelig4)
data(sanction)
z.out <- zelig(num ~ target + coop, model = "negbinom", data = sanction)
summary(z.out)
x.out <- setx(z.out)
set.seed(42)
s.out <- sim(z.out, x = x.out, num=100)
summary(s.out)

# Zelig 5 code:
data(sanction)
z5 <- znegbin$new()
z5$zelig(num ~ target + coop, data = sanction)
z5
z5$setx()
set.seed(42)
z5$sim(num=100)
statmat(z5$sim.out$x$ev[[1]])
statlevel(z5$sim.out$x$pv[[1]], 100)
z5$summarize()

