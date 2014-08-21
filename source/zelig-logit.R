
## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(Y ~ X1 + X2, model = "logit", data = mydata)
## x.out <- setx(z.out)
## s.out <- sim(z.out, x = x.out, x1 = NULL)


## ----, eval = FALSE------------------------------------------------------
## data(turnout)


## ----, eval = FALSE------------------------------------------------------
## z.out1 <- zelig(vote   age + race, model = “logit”, data = turnout)


## ----, eval = FALSE------------------------------------------------------
## x.out1 <- setx(z.out1, age = 36, race = “white”)


## ----, eval = FALSE------------------------------------------------------
## s.out1 <- sim(z.out1, x = x.out1)


## ----, eval = FALSE------------------------------------------------------
## summary(s.out1)


## ----, eval = FALSE------------------------------------------------------
## plot(s.out1)


## ----, eval = FALSE------------------------------------------------------
## z.out2 <- zelig(vote   race + educate, model = “logit”, data =
## turnout) > x.high <- setx(z.out2, educate = quantile(turnout\ :math:`educate, prob = 0.75))
## x.low <- setx(z.out2, educate = quantile(turnout`\ educate, prob = 0.25))
## 
## s.out2 <- sim(z.out2, x = x.high, x1 = x.low)
## 
## summary(s.out2)
## 
## plot(s.out2)


## ----, eval = FALSE------------------------------------------------------
## z.out1 <- zelig(vote   race + educate + age, model = “logit”, +
##    data = turnout) > z.out2 <- zelig(vote   race + educate, model =
##    “logit”, data = turnout)


## ----, eval = FALSE------------------------------------------------------
## rocplot(z.out1\ :math:`y, z.out2`\ y, fitted(z.out1),
##    fitted(z.out2))


