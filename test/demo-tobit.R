# Zelig 4 code:
library(Zelig)
data(tobin)
z.out <- zelig(durable ~ age + quant, model = "tobit", data = tobin)
summary(z.out)
x.out <- setx(z.out)
set.seed(42)
s.out1 <- sim(z.out, x = x.out)
summary(s.out1)

# Zelig 5 code:
data(tobin)
z5 <- ztobit$new()
z5$zelig(durable ~ age + quant, data = tobin, below = 1, above = 2)
z5
z5$setx()
set.seed(42)
z5$sim(num=1000)
z5$summarize()
z5$cite()

.self <- z5
i <- 1
coeff <- .self$simparam[[i]]
coeff
coeff %*% t(.self$setx.out$x[[1]])


library(AER)
fit <- tobit(durable ~ age + quant, data = tobin, left = 1, right = 2)
summary(fit)
fit$coefficients
coef(fit)

set.seed(0)
library("sampleSelection")
library("mvtnorm")

eps <- rmvnorm(500, c(0, 0), matrix(c(1, -0.7, -0.7, 1), 2, 2))
xs <- runif(500)
ys <- xs + eps[, 1] > 0
xo <- runif(500)
yoX <- xo + eps[,2]
yo <- yoX * (ys > 0)

library(VGAM)
fit1 <- vglm(formula= durable ~ age + quant, family =tobit, data = tobin)
summary(fit1)

#####code for quantile regression####
library(censReg)
##Firstly get some dummy data###
x <- rep(1:30, each=30)
y <- rep(0, times=900)
yc <- rep(0, times=900)
for (i in 1:900) {
  y[i] <- rnorm(1, mean = 0.05 * x[i] + 0.1, sd = 0.5)
  yc[i] <- max(y[i], 0)
}
fit <- lm(y ~ x);
summary(fit);
fitc <- lm(yc ~ x);
summary(fitc);
###do tobit regression####
tobitdata<-data.frame(x,yc)
fit_tobit<-censReg(yc~x, data=tobitdata,left = 0, right = Inf)
summary(fit_tobit)

coeff <- .self$simparam[[1]]


## BROKEN IN ZELIG 4
# z.out <- zelig(durable ~ age + quant, model = "tobit", data = tobin)
# x.out <- setx(z.out)
# s.out1 <- sim(z.out, x = x.out)
# summary(s.out1)
# x.high <- setx(z.out, quant = quantile(tobin$quant, prob = 0.8))
# x.low <- setx(z.out, quant = quantile(tobin$quant, prob = 0.2))
# s.out2 <- sim(z.out, x = x.high, x1 = x.low)
# summary(s.out2)
# 
# z.out <- zelig(y ~ x, model = "tobit.bayes", data)
