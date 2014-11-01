
## ----, eval = FALSE------------------------------------------------------
## z5 <- zgammagee$new()
## z5$zelig(Y ~ X1 + X2, model = "gamma.gee",
##          id = "X3", data = mydata)
## z5$setx()
## z5$sim()


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(Y ~ X1 + X2, model = "gamma.gee",
##                id = "X3", data = mydata)
## x.out <- setx(z.out)
## s.out <- sim(z.out, x = x.out)


## ----, eval = TRUE, echo = FALSE-----------------------------------------
suppressWarnings(suppressMessages(library(Zelig)))


## ----, eval = TRUE-------------------------------------------------------
data(coalition)


## ----, eval = TRUE-------------------------------------------------------
coalition$cluster <- c(rep(c(1:62), 5),rep(c(63), 4))
sorted.coalition <- coalition[order(coalition$cluster), ]


## ----, eval = TRUE-------------------------------------------------------
z.out <- zelig(duration ~ fract + numst2, model = "gamma.gee",
               id = "cluster", data = sorted.coalition,
               corstr = "exchangeable")


## ----, eval = TRUE-------------------------------------------------------
summary(z.out)


## ----, eval = TRUE-------------------------------------------------------
x.low <- setx(z.out, numst2 = 0)
x.high <- setx(z.out, numst2 = 1)


## ----, eval = TRUE-------------------------------------------------------
s.out <- sim(z.out, x = x.low, x1 = x.high)
summary(s.out)


## ----Zelig-exp, dev=c("png", "pdf"), eval = TRUE, fig.cap = "Zelig-gammagee"----
plot(s.out)


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(y ~ x, model = "gamma.gee", id, data)


