## ----setup, include=FALSE------------------------------------------------
knitr::opts_knit$set(
        stop_on_error = 2L
)

## ----zelig-5-pseudo, eval=FALSE------------------------------------------
#  z5 <- zls$new()
#  z5$zelig(Y ~ X1 + X ~ X, weights = w, data = mydata)
#  z5$setx()
#  z5$sim()
#  z5$graph()

## ----zelig-4-pseudo, eval=FALSE------------------------------------------
#  z.out <- zelig(Y ~ X1 + X2, model = "ls", weights = w, data = mydata)
#  x.out <- setx(z.out)
#  s.out <- sim(z.out, x = x.out)
#  plot(s.out)

