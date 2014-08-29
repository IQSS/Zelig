
## ----, eval = FALSE------------------------------------------------------
## z5 <- zgamma$new()
## z5$zelig(Y ~ X1 + X ~ X, data = mydata)
## z5$setx()
## z5$sim()


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(Y ~ X1 + X2, model = "gamma", data = mydata)
## x.out <- setx(z.out)
## s.out <- sim(z.out, x = x.out, x1 = NULL)


## ----, eval = TRUE, echo = FALSE-----------------------------------------
suppressWarnings(suppressMessages(library(Zelig)))


## ----, eval = TRUE-------------------------------------------------------
data(coalition)


## ----, eval = TRUE-------------------------------------------------------
z.out <- zelig(duration ~ fract + numst2, model = "gamma", data = coalition)


## ----, eval = TRUE-------------------------------------------------------
summary(z.out)


## ----, eval = TRUE-------------------------------------------------------
x.low <- setx(z.out, numst2 = 0)
x.high <- setx(z.out, numst2 = 1)


## ----, eval = TRUE-------------------------------------------------------
s.out <- sim(z.out, x = x.low, x1 = x.high)


## ----, eval = TRUE-------------------------------------------------------
summary(s.out)


## ----Zelig-gamma, dev=c("png", "pdf"), eval = TRUE, fig.cap = "Zelig-gamma"----
plot(s.out)


