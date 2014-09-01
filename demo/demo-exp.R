library(survival)
# Zelig 4 code:
library(Zelig4)
data(coalition)
z.out <- zelig(Surv(duration, ciep12) ~ fract + numst2, model = "exp",
               data = coalition)
summary(z.out)
x.low <- setx(z.out, numst2 = 0)
set.seed(42)
s.out <- sim(z.out, x = x.low, num=10)
summary(s.out)

# Zelig 5 code:
data(coalition)
z5 <- zexp$new()
z5$zelig(Surv(duration, ciep12) ~ fract + numst2, model = "exp",
         data = coalition)
z5
z5$zelig.out
z5$setx(numst2 = 0)
set.seed(42)
z5$sim(num=10)
z5$summarize()
z5$cite()

