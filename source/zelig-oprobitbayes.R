
## ----, eval = FALSE------------------------------------------------------
## z5 <- zoprobitbayes$new()
## z5$zelig(Y ~ X1 + X2, data = mydata)
## z5$setx()
## z5$sim()


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(Y ~ X1 + X2, model = "oprobit.bayes", data = mydata)
## x.out <- setx(z.out)
## s.out <- sim(z.out, x = x.out)


## ----, eval = TRUE, echo = FALSE-----------------------------------------
suppressWarnings(suppressMessages(library(Zelig)))


## ----, eval = TRUE-------------------------------------------------------
data(sanction)


## ----, eval = TRUE-------------------------------------------------------
z.out <- zelig(ncost ~ mil + coop, model = "oprobit.bayes",
               data = sanction, verbose = FALSE)


## ----, eval = TRUE-------------------------------------------------------
sanction$ncost <- factor(sanction ~ ncost, ordered = TRUE,
                         levels = c("net gain", "little effect", "modest loss",
                                     "major loss"))


## ----, eval = FALSE------------------------------------------------------
## heidel.diag(z.out$coefficients)
## raftery.diag(z.out$coefficients)


## ----, eval = FALSE------------------------------------------------------
## summary(z.out)


## ----, eval = TRUE-------------------------------------------------------
x.out <- setx(z.out)


## ----, eval = TRUE-------------------------------------------------------
s.out1 <- sim(z.out, x = x.out)
summary(s.out1)


## ----, eval = TRUE-------------------------------------------------------
x.high <- setx(z.out, mil = 0)
x.low <- setx(z.out, mil = 1)


## ----, eval = TRUE-------------------------------------------------------
s.out2 <- sim(z.out, x = x.high, x1 = x.low)
summary(s.out2)


