library(MASS)

source(file.path("..", "R", "utils.R"))
source(file.path("..", "R", "model-zelig.R"))
source(file.path("..", "R", "model-glm.R"))
source(file.path("..", "R", "model-negbinom.R"))

# Zelig 4 code:
library(Zelig)
data(sanction)
z.out <- zelig(num ~ target + coop, model = "negbinom", data = sanction)
summary(z.out)
x.out <- setx(z.out)
set.seed(42)
s.out <- sim(z.out, x = x.out, num=100)
summary(s.out)

# Zelig 5 code:
data(sanction)
z5 <- znegbinom$new()
z5$zelig(num ~ target + coop, data = sanction)
z5
z5$setx()
set.seed(42)
z5$sim(num=100)
z5$summarize()
