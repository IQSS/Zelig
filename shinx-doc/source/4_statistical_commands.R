
## ----, eval=FALSE--------------------------------------------------------
## data(turnout)
## z.out <- zelig(vote ~ race + age, model = "logit", data = turnout)
## x.out <- setx(z.out, race = "white")
## s.out <- sim(z.out, x = x.out)
## summary(s.out)


## ----, eval=FALSE--------------------------------------------------------
## data(turnout)
## z.out <- zelig(vote ~ race + educate, model = "logit", data = turnout)
## x.low <- setx(z.out, educate = 12)
## x.high <- setx(z.out, educate = 16)
## s.out <- sim(z.out, x = x.low, x1 = x.high)
## summary(s.out)
## # Numerical summary.
## plot(s.out)


## ----, eval=FALSE--------------------------------------------------------
## data(turnout)
## z.out <- zelig(vote ~ race + educate, model = "logit", data = turnout)
## x.out <- setx(z.out, fn = NULL)
## s.out <- sim(z.out, x = x.out)
## summary(s.out)


## ----, eval=FALSE--------------------------------------------------------
##  data(immi1, immi2, immi3, immi4, immi5)
## z.out <- zelig(as.factor(ipip) ~ wage1992 + prtyid + ideol, model = "ologit", data = mi(immi1, immi2, immi3, immi4, immi5))


## ----, eval=FALSE--------------------------------------------------------
## library(MatchIt)
## data(lalonde)
## m.out <- matchit(treat ~ re74 + re75 + educ + black + hispan + age, data = lalonde, method = "nearest")
## m.data <- match.data(m.out)
## z.out <- zelig(re78 ~ treat + distance + re74 + re75 + educ + black + hispan + age, data = m.data, model = "ls")
## x.out0 <- setx(z.out, fn = NULL, treat = 0)
## x.out1 <- setx(z.out, fn = NULL, treat = 1)
## s.out <- sim(z.out, x=x.out0, x1=x.out1)
## summary(s.out)


## ----, eval=FALSE--------------------------------------------------------
## library(boot)
## data(turnout)
## z.out <- zelig(vote ~ race + educate, model = "logit", data = turnout, cite=F)
## cv.out <- cv.glm(z.out, data = turnout, k=11)
## print(cv.out$delta)


