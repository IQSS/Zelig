
## ----, eval = FALSE------------------------------------------------------
## z5 <- znormal$new()
## z5$zelig(Y ~ X1 + X ~ X, data = mydata)
## z5$setx()
## z5$sim()


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(Y ~ X1 + X2, model = "normal", data = mydata)
## x.out <- setx(z.out)
## s.out <- sim(z.out, x = x.out)


## ----, eval = TRUE, echo = FALSE-----------------------------------------
suppressWarnings(suppressMessages(library(Zelig)))


## ----, eval = TRUE-------------------------------------------------------
data(macro)


## ----, eval = TRUE-------------------------------------------------------
z.out1 <- zelig(unem ~ gdp + capmob + trade, model = "normal", data = macro)


## ----, eval = TRUE-------------------------------------------------------
summary(z.out1)


## ----, eval = TRUE-------------------------------------------------------
x.high <- setx(z.out1, trade = quantile(macro$trade, 0.8))
x.low <- setx(z.out1, trade = quantile(macro$trade, 0.2))


## ----, eval = TRUE-------------------------------------------------------
s.out1 <- sim(z.out1, x = x.high, x1 = x.low)


## ----, eval = TRUE-------------------------------------------------------
summary(s.out1)


## ----Zelig-normal, dev=c("png", "pdf"), eval = TRUE, fig.cap = "Zelig-normal"----
plot(s.out1)


