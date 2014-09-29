# Zelig 4 code:
library(Zelig4)
data(macro)
z.out1 <- Zelig4::zelig(unem ~ gdp + capmob + trade, model = "normal",
                data = macro)
summary(z.out1)
x.high <- Zelig4::setx(z.out1, trade = 50)
set.seed(42)
s.out1 <- Zelig4::sim(z.out1, x = x.high)
summary(s.out1)

# Zelig 5 code:
data(macro)
z5 <- znormal$new()
z5$zelig(unem ~ gdp + capmob + trade, data=macro)
z5
z5$setx(trade = 50)
set.seed(42)
z5$sim()
z5$summarize()
z5$cite()
