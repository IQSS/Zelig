## ----, eval = FALSE------------------------------------------------------
#  z.out <- zelig(Surv(Y, C) ~ X, model = "exp", data = mydata)
#  x.out <- setx(z.out)
#  s.out <- sim(z.out, x = x.out)

## ----echo = FALSE, warning = FALSE, message = FALSE----------------------
suppressPackageStartupMessages(library(Zelig5, quietly = TRUE))
data(macro)

## ------------------------------------------------------------------------
data(coalition)

## ------------------------------------------------------------------------
z.out <- zelig(Surv(duration, ciep12) ~ fract + numst2,
               model = "exp", data = coalition)

## ------------------------------------------------------------------------
summary(z.out)

## ------------------------------------------------------------------------
x.low <- setx(z.out, numst2 = 0)
x.high <- setx(z.out, numst2 = 1)

## ------------------------------------------------------------------------
s.out <- sim(z.out, x = x.low, x1 = x.high)

## ------------------------------------------------------------------------
summary(s.out)
# plot(s.out)

