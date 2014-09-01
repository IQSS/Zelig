library(Zelig4)

data(turnout)

z.out <- zelig(vote ~ race + educate + age + I(age^2) + income,
               model = "ls", data = turnout)
summary(z.out)
x.low <- setx(z.out, educate = 12, age = 18, race = "others")
x.low
x.high <- setx(z.out, educate = 13, age = 18, race = "others")
x.high
set.seed(42)
s.out <- sim(z.out, x = x.low, x1 = x.high, num = 5)
summary(s.out)
plot(s.out)

z.out <- zelig(vote ~ race + educate + age + I(age^2) + income,
               model = "ls", data = turnout)
summary(z.out)
x.low <- setx(z.out, educate = 12:14, age = 18:22)
x.low
x.high <- setx(z.out, educate = 12:14, age = 18:22)
x.high
set.seed(42)
s.out <- sim(z.out, x = x.low, x1 = x.high, num = 5)
summary(s.out)

## Zelig 5
z5 <- zls$new()
z5$zelig(vote ~ race + educate + age + I(age^2) + income, data = turnout)
z5$zelig.out
z5$setx(educate = 12, age = 18, race = "others")
z5$setx.out
z5$setx1(educate = 13, age = 18, race = "others")
set.seed(42)
z5$sim(num = 5)
z5$summarize()


s <- list(educate = 12, age = 18, race = "others")

z5 <- zls$new()
z5$zelig(vote ~ race + educate + age + I(age^2) + income, data = turnout)
z5
z5$setrange(educate = 12:13, age = 18)
z5$setx.out
set.seed(42)
z5$sim(num = 5)
z5$summarize()

z5$setrange(educate = 12:13)
z5$setx.out

s <- list(educate = c(10, 15))
expand.grid(s)

z5$setrange(educate = c(10, 15), age = c(10, 18))

s <- list(educate = 12:13, age = 18)
expand.grid(s)

