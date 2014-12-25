# Zelig 4 code:
library(Zelig4)
data(coalition)
coalition$cluster <- c(rep(c(1:62),5),rep(c(63),4))
sorted.coalition <- coalition[order(coalition$cluster),]
z.out <- Zelig4::zelig(duration ~ fract + numst2, model = "gamma.gee", id = "cluster",
                       data = sorted.coalition)
x.low <- Zelig4::setx(z.out, numst2 = 0)
x.high <- Zelig4::setx(z.out, numst2 = 1)

s.out <- Zelig4::sim(z.out, x = x.low, x1 = x.high)
summary(s.out)

# Zelig 5 code:
set.seed(42)
z5 <- zgammagee$new()
z5$zelig(duration ~ fract + numst2, id = coalition$cluster,
         data = coalition, corstr = "exchangeable")
z5
z5$zelig.call
z5$model.call
z5$setx(numst2 = 0)
z5$setx1(numst2 = 1)
set.seed(42)
z5$sim(num=10)
z5$summarize()
z5$cite()

# geepack::geeglm(formula = duration ~ fract + numst2, family = Gamma("inverse"), 
#                 data = coalition, id = coalition$cluster, corstr = "exchangeable")
# 
# geepack::geeglm(formula = duration ~ fract + numst2, family = Gamma, 
#                 data = coalition, id = coalition$cluster, corstr = "exchangeable")

