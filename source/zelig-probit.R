
## ----, eval = FALSE------------------------------------------------------
## z5 <- zprobit$new()
## z5$zelig(Y ~ X1 + X ~ X, data = mydata)
## z5$setx()
## z5$sim()


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(Y ~ X1 + X2, model = "probit", data = mydata)
## x.out <- setx(z.out)
## s.out <- sim(z.out, x = x.out, x1 = NULL)


## ----, eval = TRUE, echo = FALSE-----------------------------------------
suppressWarnings(suppressMessages(library(Zelig)))


## ----, eval = TRUE-------------------------------------------------------
data(turnout)


## ----, eval = TRUE-------------------------------------------------------
z.out <- zelig(vote ~ race + educate, model = "probit", data = turnout)


## ----, eval = TRUE-------------------------------------------------------
summary(z.out)


## ----, eval = TRUE-------------------------------------------------------
x.out <- setx(z.out)


## ----, eval = TRUE-------------------------------------------------------
s.out <- sim(z.out, x = x.out)


## ----, eval = FALSE------------------------------------------------------
## summary(s.out)


## ----Zelig-probit, dev=c("png", "pdf"), eval = TRUE, fig.cap = "Zelig-probit"----
plot(s.out1)


