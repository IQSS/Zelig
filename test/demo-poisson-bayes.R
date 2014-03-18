source("utils.R")
source("model-zelig.R")
source("model-glm.R")
source("model-poisson-bayes.R")

p <- read.csv("http://www.ats.ucla.edu/stat/data/poisson_sim.csv")
p <- within(p, {
  prog <- factor(prog, levels = 1:3,
                 labels = c("General", "Academic", "Vocational"))
  id <- factor(id)
})

# Zelig 5 code:
set.seed(42)
z5 <- zpoissonbayes$new()
z5$zelig(num_awards ~ prog + math, data=p)
z5
z5$zelig.call
z5$model.call
z5$param(10)
z5$setx(math=40)
set.seed(42)
z5$sim(num=100)
z5$summarize()
z5$cite()

# Zelig 4 code:
library(Zelig)
z.out <- zelig(num_awards ~ prog + math, data=p, model="poisson.bayes")
summary(z.out)
x.out <- setx(z.out, math=40)
set.seed(42)
s.out <- sim(z.out, x = x.out, num=100)
summary(s.out)

