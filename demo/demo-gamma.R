# Zelig 4 code:
library(Zelig4)
data(coalition)
z.out <- zelig(duration ~ fract + numst2, model = "gamma", data = coalition)
summary(z.out)
x.low <- setx(z.out, numst2 = 0)
set.seed(42)
s.out <- sim(z.out, x = x.low, n=100000)
summary(s.out)

# Zelig 5 code:
data(coalition)
z5 <- zgamma$new()
z5$zelig(duration ~ fract + numst2, data = coalition)
z5
z5$setx(numst2 = 0)
set.seed(42)
z5$sim(num=100000)
statmat(z5$sim.out$x$ev[[1]])
statmat(z5$sim.out$x$pv[[1]])
z5$summarize()

