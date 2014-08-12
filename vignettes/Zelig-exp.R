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

