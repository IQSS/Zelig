
## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(Y ~ X1 + X2, model = "probit", data = mydata)
## x.out <- setx(z.out)
## s.out <- sim(z.out, x = x.out, x1 = NULL)


## ----, eval = FALSE------------------------------------------------------
## data(turnout)


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(vote   race + educate, model = “probit”, data =
## turnout)


## ----, eval = FALSE------------------------------------------------------
## summary(z.out)


## ----, eval = FALSE------------------------------------------------------
## x.out <- setx(z.out)


## ----, eval = FALSE------------------------------------------------------
## s.out <- sim(z.out, x = x.out)


## ----, eval = FALSE------------------------------------------------------
## summary(s.out)


