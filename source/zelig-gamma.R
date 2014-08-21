
## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(Y ~ X1 + X2, model = "gamma", data = mydata)
## x.out <- setx(z.out)
## s.out <- sim(z.out, x = x.out, x1 = NULL)


## ----, eval = FALSE------------------------------------------------------
## data(coalition)


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(duration   fract + numst2, model = “gamma”, data =
## coalition)


## ----, eval = FALSE------------------------------------------------------
## summary(z.out)


## ----, eval = FALSE------------------------------------------------------
## x.low <- setx(z.out, numst2 = 0) RRR> x.high <- setx(z.out, numst2
## = 1)


## ----, eval = FALSE------------------------------------------------------
## s.out <- sim(z.out, x = x.low, x1 = x.high)


## ----, eval = FALSE------------------------------------------------------
## summary(s.out)


## ----, eval = FALSE------------------------------------------------------
## plot(s.out)


