# source("utils.R")
# source("model-zelig.R")
# source("model-glm.R")
# source("model-normal.R")

# Zelig 5 code:
data(macro)
z5 <- znormal$new()
z5$zelig(unem ~ gdp + capmob + trade, data=macro)
z5
z5$setx(trade = 50)
set.seed(42)
z5$sim(num=1000)
z5$summarize()
z5$cite()

# Zelig 4 code:
library(Zelig)
data(macro)
z.out1 <- zelig(unem ~ gdp + capmob + trade, model = "normal",
                data = macro)
summary(z.out1)
x.high <- setx(z.out1, trade = 50)
set.seed(42)
s.out1 <- sim(z.out1, x = x.high)
summary(s.out1)
