
## ----, eval = FALSE------------------------------------------------------
## z5 <- zgammagee$new()
## z5$zelig(Y ~ X1 + X2, model = "probit.gee",
##          id = "X3", data = mydata)
## z5$setx()
## z5$sim()


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(Y ~ X1 + X2, model = "probit.gee",
##                id = "X3", data = mydata)
## x.out <- setx(z.out)
## s.out <- sim(z.out, x = x.out)


## ----, eval = TRUE, echo = FALSE-----------------------------------------
suppressWarnings(suppressMessages(library(Zelig)))


## ----, eval = TRUE-------------------------------------------------------
data(turnout)


## ----, eval = TRUE-------------------------------------------------------
turnout$cluster <- rep(c(1:200), 10)
sorted.turnout <- turnout[order(turnout$cluster), ]


## ----, eval = TRUE-------------------------------------------------------
z.out1 <- zelig(vote ~ race + educate, model = "probit.gee",
                id = "cluster", data = sorted.turnout)


## ----, eval = TRUE-------------------------------------------------------
x.out1 <- setx(z.out1)


## ----, eval = TRUE-------------------------------------------------------
s.out1 <- sim(z.out1, x = x.out1)


## ----, eval = TRUE-------------------------------------------------------
summary(s.out1)


## ----Zelig-probitgee1, dev=c("png", "pdf"), eval = TRUE, fig.cap = "Zelig-probitgee1"----
plot(s.out1)


## ----, eval = TRUE-------------------------------------------------------
x.high <- setx(z.out1, educate = quantile(turnout$educate, prob = 0.75))
x.low <- setx(z.out1, educate = quantile(turnout$educate, prob = 0.25))


## ----, eval = TRUE-------------------------------------------------------
s.out2 <- sim(z.out1, x = x.high, x1 = x.low)


## ----, eval = TRUE-------------------------------------------------------
summary(s.out2)


## ----Zelig-probitgee2, dev=c("png", "pdf"), eval = TRUE, fig.cap = "Zelig-probitgee2"----
plot(s.out2)


## ----, eval = TRUE-------------------------------------------------------
corr.mat <- matrix(rep(0.5, 100), nrow = 10, ncol = 10)
diag(corr.mat) <- 1
corr.mat <- fixed2Zcor(corr.mat, id=sorted.turnout$cluster, waves=sorted.turnout$race)


## ----, eval = TRUE-------------------------------------------------------
z.out2 <- zelig(vote ~ race + educate, model = "probit.gee",
                id = "cluster", data = sorted.turnout,
                corstr = "fixed", zcor = corr.mat)


## ----, eval = FALSE------------------------------------------------------
## summary(z.out2)


