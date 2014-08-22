
## ----, eval = FALSE------------------------------------------------------
## data(tobin)


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(durable   age + quant, model = “tobit”, data = tobin)


## ----, eval = FALSE------------------------------------------------------
## x.out <- setx(z.out)


## ----, eval = FALSE------------------------------------------------------
## s.out1 <- sim(z.out, x = x.out)


## ----, eval = FALSE------------------------------------------------------
## summary(s.out1)


## ----, eval = FALSE------------------------------------------------------
## x.high <- setx(z.out, quant = quantile(tobin\ :math:`quant, prob = 0.8))
## x.low <- setx(z.out, quant = quantile(tobin`\ quant, prob = 0.2))


## ----, eval = FALSE------------------------------------------------------
## s.out2 <- sim(z.out, x = x.high, x1 = x.low)


## ----, eval = FALSE------------------------------------------------------
## summary(s.out2)


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(y ~ x, model = "tobit", data)


