data(CPS1988)
z5 <- zquantile$new()
z5$zelig(log(wage) ~ experience + I(experience^2) + education, data = CPS1988, tau = 0.75)
z5
z5$zelig.out
z5$setx(education = 15)
z5$setx1(education = 10)
z5$sim(num = 10)
z5$summarize()
z5$graph()

data(stackloss)
z.out1 <- zquantile$new()
z.out1$zelig(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.,
             data = stackloss, tau = 0.5)

z.out1 <- zelig(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.,
                model = "rq", data = stackloss,
                tau = 0.5)

quantreg::rq(formula = log(wage) ~ experience + I(experience^2) + 
               education, data = CPS1988, tau = 0.75)
z.out1
summary(z.out1$zelig.out$z.out[[1]])
x.high <- z.out1$setx(Water.Temp = quantile(stackloss$Water.Temp, 0.8))
x.low <- z.out1$setx1(Water.Temp = quantile(stackloss$Water.Temp, 0.2))
z.out1$sim(num = 10000)
z.out1$summarize()

data(macro)
z.out2 <- zquantile$new()
z.out2$zelig(unem ~ gdp + trade + capmob + as.factor(country), tau = 0.5, data = macro)
z.out2$setx(country = "United States")
z.out2$setx1(country = "Japan")
z.out2$sim()
z.out1$summarize()

data(engel)
z.out3 <- zquantile$new()
z.out3$zelig(foodexp ~ income, tau = seq(0.1, 0.3, by = 0.1), data = engel)
z.out3$setx()
z.out3$sim()

# r <- .self$data %>% 
#   group_by(tau) %>%
#   do(model = rq(foodexp ~ income, data = ., tau = .$tau[1]))

library(Zelig4)
data(engel)
z.out <- zelig(foodexp ~ income, tau = seq(0.1, 0.9, by = 0.1), data = engel, model = "rq")
z.out3$setx()
z.out3$sim()

data(engel)
z.out4 <- zquantile$new()
z.out4$zelig(foodexp ~ income, data = engel, by = )
z.out4$setx()
z.out4$sim()


summary(z.out3$zelig.out[[1]])
plot(summary(z.out3$zelig.out[[1]]))

z.out3$setx(income = quantile(engel$income, 0.25))
z.out3$setx1(income = quantile(engel$income, 0.75))

z.out3$sim()

fit <- rq(foodexp ~ income, tau = seq(0.1, 0.9, by = 0.1), data = engel)
plot(summary(fit))


#####
# data(engel)
# z.out3 <- zelig(foodexp ~ income, model = "quantile",
#                 tau = seq(0.1,0.9,by=0.1), data = engel)
# summary(z.out3)
# plot(summary(z.out3))
# plot(z.out3)
# 
# x.bottom <- setx(z.out3, income=quantile(engel$income, 0.25))
# x.top <- setx(z.out3, income=quantile(engel$income, 0.75))
# 
# s.out3 <- sim(z.out3, x = x.bottom, x1 = x.top)
# 
# summary(s.out3)
