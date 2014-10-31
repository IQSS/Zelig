# Zelig 4 code:
library(Zelig4)
data(tobin)
z.out <- zelig(durable ~ age + quant, model = "tobit", data = tobin)
summary(z.out)
x.out <- setx(z.out)
set.seed(42)
s.out1 <- sim(z.out, x = x.out)
summary(s.out1)

# Zelig 5 code:
data(tobin)
z5 <- ztobit$new()
z5$zelig(durable ~ age + quant, data = tobin, below = 1, above = 20)
z5
z5$setx()
set.seed(42)
z5$sim(num=10000)
statmat(z5$sim.out$x$ev[[1]])
statmat(z5$sim.out$x$pv[[1]])
z5$summarize()
z5$cite()

# library(AER)  
# fit <- tobit(durable ~ age + quant, data = tobin, left = 1, right = 2)
# summary(fit)
# fit$coefficients
# coef(fit)
