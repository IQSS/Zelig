## ----, eval = FALSE------------------------------------------------------
#  z.out <- zelig(Y ~ X1 + X2, model = "ls", data = mydata)
#  x.out <- setx(z.out)
#  s.out <- sim(z.out, x = x.out)

## ----echo = FALSE, warning = FALSE, message = FALSE----------------------
suppressPackageStartupMessages(library(Zelig5, quietly = TRUE))
data(macro)

## ------------------------------------------------------------------------
data(macro)

## ------------------------------------------------------------------------
z.out1 <- zelig(unem ~ gdp + capmob + trade, model = "ls", data = macro)

## ------------------------------------------------------------------------
summary(z.out1)

## ------------------------------------------------------------------------
x.high <- setx(z.out1, trade = quantile(macro$trade, 0.8))
x.low <- setx(z.out1, trade = quantile(macro$trade, 0.2))

## ------------------------------------------------------------------------
s.out1 <- sim(z.out1, x = x.high, x1 = x.low)

## ------------------------------------------------------------------------
z.out2 <- zelig(unem ~ gdp + trade + capmob + country,
                model = "ls", data = macro)

z.out3 <- zelig(unem ~ gdp + trade + capmob + as.factor(country),
                model = "ls", data = macro)
x.US <- setx(z.out3, country = "United States")

## ------------------------------------------------------------------------
x.US <- setx(z.out2, country = "United States")
x.Japan <- setx(z.out2, country = "Japan")

## ------------------------------------------------------------------------
s.out2 <- sim(z.out2, x.US, x.Japan)

