
## ----, eval = FALSE------------------------------------------------------
## z5 <- zlogit$new()
## z5$zelig(Y ~ X1 + X ~ X, data = mydata)
## z5$setx()
## z5$sim()


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(Y ~ X1 + X2, model = "logit", data = mydata)
## x.out <- setx(z.out)
## s.out <- sim(z.out, x = x.out, x1 = NULL)


## ----, eval = TRUE, echo = FALSE-----------------------------------------
suppressWarnings(suppressMessages(library(Zelig)))


## ----, eval = TRUE-------------------------------------------------------
data(turnout)


## ----, eval = TRUE-------------------------------------------------------
z.out1 <- zelig(vote ~ age + race, model = "logit", data = turnout)


## ----, eval = TRUE-------------------------------------------------------
x.out1 <- setx(z.out1, age = 36, race = "white")


## ----, eval = TRUE-------------------------------------------------------
s.out1 <- sim(z.out1, x = x.out1)


## ----, eval = TRUE-------------------------------------------------------
summary(s.out1)


## ----Zelig-logit-1, dev=c("png", "pdf"), eval = TRUE, fig.cap = "Zelig-logit-1"----
plot(s.out1)


## ----, eval = TRUE-------------------------------------------------------
z.out2 <- zelig(vote ~ race + educate, model = "logit", data = turnout)
x.high <- setx(z.out2, educate = quantile(turnout$educate, prob = 0.75))
x.low <- setx(z.out2, educate = quantile(turnout$educate, prob = 0.25))
s.out2 <- sim(z.out2, x = x.high, x1 = x.low)
summary(s.out2)


## ----Zelig-logit-2, dev=c("png", "pdf"), eval = TRUE, fig.cap = "Zelig-logit-2"----
plot(s.out2)


