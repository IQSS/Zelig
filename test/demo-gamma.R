# source("utils.R")
# source("model-zelig.R")
# source("model-glm.R")
# source("model-gamma.R")

# Zelig 5 code:
coalition <- read.csv('data-coalition.csv')
z5 <- zgamma$new()
z5$zelig(duration ~ fract + numst2, data = coalition)
z5
z5$setx(numst2 = 0)
set.seed(42)
z5$sim(num=1000)
z5$summarize()

# Zelig 4 code:
library(Zelig)
data(coalition)
z.out <- zelig(duration ~ fract + numst2, model = "gamma", data = coalition)
summary(z.out)
x.low <- setx(z.out, numst2 = 0)
set.seed(42)
s.out <- sim(z.out, x = x.low, n=1000)
summary(s.out)
