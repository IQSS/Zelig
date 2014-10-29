
## ----, eval = FALSE------------------------------------------------------
## z5 <- zpoissonbayes$new()
## z5$zelig(Y ~ X1 + X2, data = mydata)
## z5$setx()
## z5$sim()


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(Y ~ X1 + X2, model = "poisson.bayes", data = mydata)
## x.out <- setx(z.out)
## s.out <- sim(z.out, x = x.out)


## ----, eval = TRUE, echo = FALSE-----------------------------------------
suppressWarnings(suppressMessages(library(Zelig)))


## ----, eval = TRUE-------------------------------------------------------
data(sanction)


## ----, eval = TRUE-------------------------------------------------------
z.out <- zelig(num ~ target + coop, model = "poisson.bayes",
               data = sanction, verbose = FALSE)


## ----, eval = FALSE------------------------------------------------------
## geweke.diag(z.out$coefficients)


## ----, eval = FALSE------------------------------------------------------
## heidel.diag(z.out`\ coefficients)


## ----, eval = FALSE------------------------------------------------------
## raftery.diag(z.out..coefficients)


## ----, eval = FALSE------------------------------------------------------
## summary(z.out)


## ----, eval = TRUE-------------------------------------------------------
x.out <- setx(z.out)


## ----, eval = TRUE-------------------------------------------------------
s.out1 <- sim(z.out, x = x.out)


## ----, eval = FALSE------------------------------------------------------
## summary(s.out1)


## ----, eval = TRUE-------------------------------------------------------
x.max <- setx(z.out, target = max(sanction$target))
x.min <- setx(z.out, target = min(sanction$target))


## ----, eval = TRUE-------------------------------------------------------
s.out2 <- sim(z.out, x = x.max, x1 = x.min)
summary(s.out2)


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(y ~ x, model = "poisson.bayes", data)


