
## ----, eval = FALSE------------------------------------------------------
## z5 <- zprobitbayes$new()
## z5$zelig((Y ~ X1 + X2, below = 0, above = Inf, data = mydata)
## z5$setx()
## z5$sim()


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(Y ~ X1 + X2, below = 0, above = Inf,
##                model = "tobit.bayes", data = mydata)
## x.out <- setx(z.out)
## s.out <- sim(z.out, x = x.out)


## ----, eval = TRUE, echo = FALSE-----------------------------------------
suppressWarnings(suppressMessages(library(Zelig)))


## ----, eval = TRUE-------------------------------------------------------
data(tobin)


## ----, eval = TRUE-------------------------------------------------------
z.out <- zelig(durable ~ age + quant, model = "tobit.bayes",
               data = tobin, verbose = FALSE)


## ----, eval = FALSE------------------------------------------------------
## geweke.diag(z.out$coefficients)


## ----, eval = FALSE------------------------------------------------------
## heidel.diag(z.out$coefficients)


## ----, eval = FALSE------------------------------------------------------
## raftery.diag(z.out$coefficients)


## ----, eval = FALSE------------------------------------------------------
## summary(z.out)


## ----, eval = TRUE-------------------------------------------------------
x.out <- setx(z.out)


## ----, eval = TRUE-------------------------------------------------------
s.out1 <- sim(z.out, x = x.out)


## ----, eval = FALSE------------------------------------------------------
## summary(s.out1)


## ----, eval = TRUE-------------------------------------------------------
x.high <- setx(z.out, quant = quantile(tobin$quant, prob = 0.8))
x.low <- setx(z.out, quant = quantile(tobin$quant, prob = 0.2))


## ----, eval = TRUE-------------------------------------------------------
s.out2 <- sim(z.out, x = x.high, x1 = x.low)


## ----, eval = FALSE------------------------------------------------------
## summary(s.out2)


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(y ~ x, model = "tobit.bayes", data)


