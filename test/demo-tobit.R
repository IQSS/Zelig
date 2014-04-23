source(file.path("..", "R", "utils.R"))
source(file.path("..", "R", "model-zelig.R"))
source(file.path("..", "R", "model-tobit.R"))

# Zelig 4 code:
library(Zelig)
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
z5$zelig(durable ~ age + quant, data = tobin)
z5
z5$setx()
set.seed(42)
z5$sim(num=10)
z5$summarize()
z5$cite()
