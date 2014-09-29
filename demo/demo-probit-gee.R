# Zelig 4 code:
data(turnout)
turnout$cluster <- rep(c(1:200), 10)
sorted.turnout <- turnout[order(turnout$cluster), ]
z.out1 <- Zelig4::zelig(vote ~ race + educate, model = "probit.gee", id = "cluster",
                        data = sorted.turnout, corstr = "stat_M_dep",
                        Mv = 3)
x.out1 <- Zelig4::setx(z.out1)
set.seed(42)
s.out1 <- Zelig4::sim(z.out1, x = x.out1)
summary(s.out1)

# Zelig 5 code:
z5 <- zprobitgee$new()
z5$zelig(vote ~ race + educate, id = "cluster",
         data = sorted.turnout, corstr = "unstructured")

z5
z5$zelig.call
z5$model.call
z5$setx()
set.seed(42)
z5$sim()
z5$summarize()
z5$cite()
