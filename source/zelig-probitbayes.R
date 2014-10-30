
## ----, eval = FALSE------------------------------------------------------
## z5 <- zprobitbayes$new()
## z5$zelig(Y ~ X1 + X2, data = mydata)
## z5$setx()
## z5$sim()


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(Y ~ X1 + X2, model = "probit.bayes", data = mydata)
## x.out <- setx(z.out)
## s.out <- sim(z.out, x = x.out)


## ----, eval = TRUE, echo = FALSE-----------------------------------------
suppressWarnings(suppressMessages(library(Zelig)))


## ----, eval = TRUE-------------------------------------------------------
data(turnout)


## ----, eval = TRUE-------------------------------------------------------
z.out <- zelig(vote ~ race + educate, model = "probit.bayes", 
               data = turnout, verbose = FALSE)


## ----, eval = FALSE------------------------------------------------------
## geweke.diag(z.out coefficients)


## ----, eval = FALSE------------------------------------------------------
## heidel.diag(z.out coefficients)


## ----, eval = FALSE------------------------------------------------------
## raftery.diag(z.out coefficients)


## ----, eval = FALSE------------------------------------------------------
## summary(z.out)


## ----, eval = TRUE-------------------------------------------------------
x.out <- setx(z.out)


## ----, eval = TRUE-------------------------------------------------------
s.out1 <- sim(z.out, x = x.out)


## ----, eval = FALSE------------------------------------------------------
## summary(s.out1)


## ----, eval = TRUE-------------------------------------------------------
x.high <- setx(z.out, educate = quantile(turnout$educate, prob = 0.75))
x.low <- setx(z.out, educate = quantile(turnout$educate, prob = 0.25))


## ----, eval = TRUE-------------------------------------------------------
s.out2 <- sim(z.out, x = x.high, x1 = x.low)


## ----, eval = FALSE------------------------------------------------------
## summary(s.out2)


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(y ~ x, model = "probit.bayes", data)


