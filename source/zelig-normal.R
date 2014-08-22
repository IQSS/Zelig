
## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(Y ~ X1 + X2, model = "normal", data = mydata)
## x.out <- setx(z.out)
## s.out <- sim(z.out, x = x.out)


## ----, eval = FALSE------------------------------------------------------
## data(macro)


## ----, eval = FALSE------------------------------------------------------
## z.out1 <- zelig(unem   gdp + capmob + trade, model = “normal”, + data
## = macro)


## ----, eval = FALSE------------------------------------------------------
## summary(z.out1)


## ----, eval = FALSE------------------------------------------------------
## x.high <- setx(z.out1, trade = quantile(macro\ :math:`trade, 0.8))
## x.low <- setx(z.out1, trade = quantile(macro`\ trade, 0.2))


## ----, eval = FALSE------------------------------------------------------
## s.out1 <- sim(z.out1, x = x.high, x1 = x.low)


## ----, eval = FALSE------------------------------------------------------
## summary(s.out1)


## ----, eval = FALSE------------------------------------------------------
## plot(s.out1)


