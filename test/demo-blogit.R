# Zelig 4 code:
library(Zelig)
library(ZeligChoice)
data(sanction)
z.out1 <- zelig(cbind(import, export) ~ coop + cost + target,
                model = "blogit", data = sanction)
summary(z.out1)
x.low <- setx(z.out1, cost = 1)
set.seed(42)
s.out1 <- sim(z.out1, x.low, num=100)
summary(s.out1)

# Zelig 5 code:
data(sanction)
z5 <- zblogit$new()
z5$zelig(cbind(import, export) ~ coop + cost + target, data = sanction)
z5
z5$setx(cost=1)
set.seed(42)
z5$sim(num=100)
z5$summarize()
z5$cite()

