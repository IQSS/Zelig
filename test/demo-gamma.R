source(file.path("..", "R", "utils.R"))
source(file.path("..", "R", "model-zelig.R"))
source(file.path("..", "R", "model-glm.R"))
source(file.path("..", "R", "model-gamma.R"))

# Zelig 4 code:
library(Zelig)
data(coalition)
z.out <- zelig(duration ~ fract + numst2, model = "gamma", data = coalition)
summary(z.out)
x.low <- setx(z.out, numst2 = 0)
set.seed(42)
s.out <- sim(z.out, x = x.low, n=1000)
summary(s.out)

# Zelig 5 code:
data(coalition)
z5 <- zgamma$new()
z5$zelig(duration ~ fract + numst2, data = coalition)
z5
z5$setx(numst2 = 0)
set.seed(42)
z5$sim(num=1000)
z5$summarize()

