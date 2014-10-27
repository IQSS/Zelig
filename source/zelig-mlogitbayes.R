
## ----, eval = FALSE------------------------------------------------------
## z5 <- zmlogitbayes$new()
## z5$zelig(Y ~ X1 + X2, data = mydata)
## z5$setx()
## z5$sim()


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(Y ~ X1 + X2, model = "mlogit.bayes", data = mydata)
## x.out <- setx(z.out)
## s.out <- sim(z.out, x = x.out)


## ----, eval = TRUE, echo = FALSE-----------------------------------------
suppressPackageStartupMessages(suppressWarnings(library(Zelig)))


## ----, eval = TRUE-------------------------------------------------------
data(mexico)


## ----, eval = TRUE-------------------------------------------------------
z.out <- zelig(vote88 ~ pristr + othcok + othsocok,
               model = "mlogit.bayes", data = mexico,
               verbose = FALSE)


## ----, eval = FALSE------------------------------------------------------
## raftery.diag(z.out$coefficients)


## ----, eval = FALSE------------------------------------------------------
## summary(z.out)


## ----, eval = TRUE-------------------------------------------------------
x.out <- setx(z.out)


## ----, eval = TRUE-------------------------------------------------------
s.out1 <- sim(z.out, x = x.out)
summary(s.out1)


## ----, eval = TRUE-------------------------------------------------------
x.weak <- setx(z.out, pristr = 1)
x.strong <- setx(z.out, pristr = 3)
s.out2 <- sim(z.out, x = x.strong, x1 = x.weak)
summary(s.out2)


## ----, eval = FALSE------------------------------------------------------
##     z.out <- zelig(y ~ x, model = "mlogit.bayes", data)


