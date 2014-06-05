# source(file.path("..", "R", "utils.R"))
# source(file.path("..", "R", "model-zelig.R"))
# source(file.path("..", "R", "model-glm.R"))
# source(file.path("..", "R", "model-poisson.R"))

p <- read.csv("http://www.ats.ucla.edu/stat/data/poisson_sim.csv")
p <- within(p, {
  prog <- factor(prog, levels = 1:3,
                 labels = c("General", "Academic", "Vocational"))
  id <- factor(id)
})
head(p)

library(MASS)

w <- runif(200)

# Zelig 4 code:
library(Zelig)
z.out <- zelig(num_awards ~ prog + math, data=p, model="poisson",
               weights=w)
summary(z.out)
x.out <- setx(z.out, math = 40)
set.seed(42)
s.out <- sim(z.out, x = x.out, num = 1000)
summary(s.out)

# Zelig 5 code:
z5 <- zpoisson$new()
z5$zelig(num_awards ~ prog + math, data=p,
         weights=w)
z5
z5$zelig.call
z5$model.call
z5$setx(math = 40)
set.seed(42)
z5$sim(num = 1000)
z5$summarize()
z5$cite()


