# Zelig 4 code:
library(Zelig4)
data(sanction)
sanction$cluster <- c(rep(c(1:15), 5), rep(c(16), 3))
sorted.sanction <- sanction[order(sanction$cluster), ]
z.out <- Zelig4::zelig(num ~ target + coop, model = "poisson.gee", id = "cluster",
                       data = sorted.sanction, robust = FALSE, corstr = "exchangeable")
summary(z.out)
x.out <- Zelig4::setx(z.out)
set.seed(42)
s.out <- Zelig4::sim(z.out, x.out)
summary(s.out)

# Zelig 5 code:
set.seed(42)
z5 <- zpoissongee$new()
z5$zelig(num ~ target + coop, id = "cluster",
         data = sorted.sanction, corstr = "exchangeable")
z5
z5$zelig.call
z5$model.call
z5$setx()
set.seed(42)
z5$sim()
z5$summarize()
z5$cite()

