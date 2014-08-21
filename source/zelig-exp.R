
## ----, eval = FALSE------------------------------------------------------
## 
##     > z.out <- zelig(Surv(Y, C) ~ X, model = "exp", data = mydata)
##     > x.out <- setx(z.out)
##     > s.out <- sim(z.out, x = x.out)


## ----, eval = FALSE------------------------------------------------------
## data(coalition)


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(Surv(duration, ciep12)   fract + numst2, model = “exp”, + data = coalition)
## 
## 
## View the regression output:
## 

## ----, eval = FALSE------------------------------------------------------
## summary(z.out)


## ----, eval = FALSE------------------------------------------------------
## x.low <- setx(z.out, numst2 = 0) RRR> x.high <- setx(z.out, numst2 = 1)


## ----, eval = FALSE------------------------------------------------------
## s.out <- sim(z.out, x = x.low, x1 = x.high)


## ----, eval = FALSE------------------------------------------------------
## summary(s.out)


## ----, eval = FALSE------------------------------------------------------
## plot(s.out)


