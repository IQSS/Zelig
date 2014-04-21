source(file.path("..", "R", "utils.R"))
source(file.path("..", "R", "model-zelig.R"))
source(file.path("..", "R", "model-glm.R"))
source(file.path("..", "R", "model-poisson-gee.R"))

# Zelig 4 code:
library(Zelig)
data(sanction)
sanction$cluster <- c(rep(c(1:15), 5), rep(c(16), 3))
sorted.sanction <- sanction[order(sanction$cluster), ]
z.out <- zelig(num ~ target + coop, model = "poisson.gee", id = "cluster",
               data = sorted.sanction, robust = FALSE, corstr = "exchangeable")
summary(z.out)
x.out <- setx(z.out)
set.seed(42)
s.out <- sim(z.out, x.out, num=10)
summary(s.out)

# Zelig 5 code:
library(gee)
set.seed(42)
z5 <- zpoissongee$new()
z5$zelig(num ~ target + coop, id = "cluster",
         data = sorted.sanction, corstr = "exchangeable")
z5
z5$zelig.call
z5$model.call
z5$param(10)
z5$setx()
set.seed(42)
z5$sim(num=10)
z5$summarize()
z5$cite()


