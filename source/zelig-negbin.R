
## ----, eval = FALSE------------------------------------------------------
## z5 <- znegbin$new()
## z5$zelig(Y ~ X1 + X ~ X, data = mydata)
## z5$setx()
## z5$sim()


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(Y ~ X1 + X2, model = "negbin", data = mydata)
## x.out <- setx(z.out)
## s.out <- sim(z.out, x = x.out)


## ----, eval = TRUE, echo = FALSE-----------------------------------------
suppressWarnings(suppressMessages(library(Zelig)))


## ----, eval = TRUE-------------------------------------------------------
data(sanction)


## ----, eval = TRUE-------------------------------------------------------
z.out <- zelig(num ~ target + coop, model = "negbin", data = sanction)


## ----, eval = TRUE-------------------------------------------------------
summary(z.out)


## ----, eval = TRUE-------------------------------------------------------
x.out <- setx(z.out)


## ----, eval = TRUE-------------------------------------------------------
s.out <- sim(z.out, x = x.out)


## ----, eval = TRUE-------------------------------------------------------
summary(s.out)


## ----Zelig-negbin, dev=c("png", "pdf"), eval = TRUE, fig.cap = "Zelig-negbin"----
plot(s.out)


