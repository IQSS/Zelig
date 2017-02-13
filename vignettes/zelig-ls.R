## ----setup, include=FALSE------------------------------------------------
knitr::opts_knit$set(
        stop_on_error = 2L
)

## ---- eval = FALSE-------------------------------------------------------
#  z5 <- zls$new()
#  z5$zelig(Y ~ X1 + X ~ X, weights = w, data = mydata)
#  z5$setx()
#  z5$sim()

## ---- eval = FALSE-------------------------------------------------------
#  z.out <- zelig(Y ~ X1 + X2, model = "ls", weights = w, data = mydata)
#  x.out <- setx(z.out)
#  s.out <- sim(z.out, x = x.out)

## ------------------------------------------------------------------------
library(Zelig)
data(macro)

## ----message=FALSE-------------------------------------------------------
z.out1 <- zelig(unem ~ gdp + capmob + trade, model = "ls", data = macro, 
                cite = FALSE)

## ---- eval = TRUE--------------------------------------------------------
summary(z.out1)

## ---- eval = TRUE--------------------------------------------------------
x.high <- setx(z.out1, trade = quantile(macro$trade, 0.8))
x.low <- setx(z.out1, trade = quantile(macro$trade, 0.2))

## ---- eval = TRUE--------------------------------------------------------
s.out1 <- sim(z.out1, x = x.high, x1 = x.low)

summary(s.out1)

## ------------------------------------------------------------------------
#plot(s.out1)
#s.out1$graph()

## ------------------------------------------------------------------------
z.out2 <- zelig(unem ~ gdp + trade + capmob + as.factor(country), model = "ls", 
                data = macro)

## ------------------------------------------------------------------------
x.US <- setx(z.out2, country = "United States")
x.Japan <- setx(z.out2, country = "Japan")

## ------------------------------------------------------------------------
s.out2 <- sim(z.out2, x = x.US, x1 = x.Japan)

#plot(s.out2)

## ------------------------------------------------------------------------
z.out1$getcoef() 

## ------------------------------------------------------------------------
z.out1$getvcov()

## ------------------------------------------------------------------------
z.out1$summarize()

## ---- eval=FALSE---------------------------------------------------------
#  summary(z.out1)

