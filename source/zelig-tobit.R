
## ----, eval = FALSE------------------------------------------------------
## z5 <- ztobit$new()
## z5$zelig(Y ~ X1 + X2, below = 0, above = Inf, data = mydata)
## z5$setx()
## z5$sim()


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(Y ~ X1 + X2, below = 0, above = Inf, model = "tobit", data = mydata)
## x.out <- setx(z.out)
## s.out <- sim(z.out, x = x.out)


## ----, eval = TRUE, echo = FALSE-----------------------------------------
suppressWarnings(suppressMessages(library(Zelig)))


## ----, eval = TRUE-------------------------------------------------------
data(tobin)


## ----, eval = TRUE-------------------------------------------------------
z.out <- zelig(durable ~ age + quant, model = "tobit", data = tobin)


## ----, eval = TRUE-------------------------------------------------------
x.out <- setx(z.out)


## ----, eval = TRUE-------------------------------------------------------
s.out1 <- sim(z.out, x = x.out)


## ----, eval = TRUE-------------------------------------------------------
summary(s.out1)


## ----, eval = TRUE-------------------------------------------------------
x.high <- setx(z.out, quant = quantile(tobin$quant, prob = 0.8))
x.low <- setx(z.out, quant = quantile(tobin$quant, prob = 0.2))


## ----, eval = TRUE-------------------------------------------------------
s.out2 <- sim(z.out, x = x.high, x1 = x.low)


## ----, eval = TRUE-------------------------------------------------------
summary(s.out2)


## ----Zelig-tobit, dev=c("png", "pdf"), eval = TRUE, fig.cap = "Zelig-tobit"----
plot(s.out1)


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(y ~ x, model = "tobit", data)


