library(Amelia)
data(africa)
df <- head(africa, 100)
imp <- amelia(x = df , cs = "country", m = 5) 
imp2 <- imp$imputations

# Zelig 4 code:
library(Zelig4)
z.out <- zelig(infl ~ trade + civlib, data = imp, model = "ls")
summary(z.out)
x.out <- setx(z.out, civlib = .5)
set.seed(42)
s.out <- sim(z.out, x.out, num = 100)
summary(s.out)

# Zelig 5 code:
# library(data.table)
library(Amelia)
library(Zelig)
z5 <- zls$new()
z5$zelig(infl ~ trade + civlib, data = imp)
z5$zelig(infl ~ trade + civlib, data = imp, by = "country")
z5
z5$zelig.out
z5$model.call
z5$zelig.call
z5$setx(civlib = .5)
z5$setrange(civlib = c(0.3, .5))
z5$setx.out
set.seed(42)
z5$sim(num=1000)
z5$sim.out
z5$summarize()
z5$cite()

z5$setrange(speed = 30:32,speed = 30:32)
z5$setx.out
set.seed(42)
z5$sim(num=3)
z5$sim.out
z5$summarize()

data(freetrade)
df <- head(freetrade, 1000)
imp <- amelia(x = df, cs = "country", m = 10) 
imp2 <- imp$imputations


