
## ----, eval = FALSE------------------------------------------------------
## z5 <- zweibull$new()
## z5$zelig(Surv(Y, C) ~ X, data = mydata)
## z5$setx()
## z5$sim()


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(Surv(Y, C) ~ X, model = "weibull", data = mydata)
## x.out <- setx(z.out)
## s.out <- sim(z.out, x = x.out)


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(y ~ x1 + x2, robust = TRUE, cluster = "x3",
##                model = "exp", data = mydata)


## ----, eval = TRUE-------------------------------------------------------
data(coalition)


## ----, eval = TRUE-------------------------------------------------------
z.out <- zelig(Surv(duration, ciep12) ~ fract + numst2,
               model = "weibull", data = coalition)


## ----, eval = TRUE-------------------------------------------------------
summary(z.out)


## ----, eval = TRUE-------------------------------------------------------
x.low <- setx(z.out, numst2 = 0)
x.high <- setx(z.out, numst2 = 1)


## ----, eval = TRUE-------------------------------------------------------
s.out <- sim(z.out, x = x.low, x1 = x.high)


## ----, eval = TRUE-------------------------------------------------------
summary(s.out)


## ----Zelig-weibull, dev=c("png", "pdf"), eval = TRUE, fig.cap = "Zelig-weibull"----
plot(s.out)


