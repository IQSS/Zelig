library(data.table)

data(cars)
Cars <- data.table(cars)
z5 <- zls$new()
z5$zelig(dist ~ speed, data = Cars)
z5$setx1(speed = 30)
z5$setx()
set.seed(42)
z5$sim()
z5$summarize()
z5$cite()

library(Amelia)
data(africa)
Africa <- data.table(africa)
imp <- amelia(x = Africa , cs = "country", m = 5) 

z5 <- zls$new()
z5$zelig(infl ~ trade + civlib, data = imp)

