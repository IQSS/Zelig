library(MASS)

# source("utils.R")
# source("model-zelig.R")
# source("model-glm.R")
# source("model-negbinom.R")

# Zelig 5 code:
data(sanction)
z5 <- znegbinom$new()
z5$zelig(num ~ target + coop, data = sanction)
z5
z5$setx()
set.seed(42)
z5$sim(num=1000)
z5$summarize()

# Zelig 4 code:
library(Zelig)
data(sanction)
z.out <- zelig(num ~ target + coop, model = "negbinom", data = sanction)
summary(z.out)
x.out <- setx(z.out)
set.seed(42)
s.out <- sim(z.out, x = x.out)
summary(s.out)
