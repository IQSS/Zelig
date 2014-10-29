library(Zelig4)
data(turnout)
turnout$dedu <- ifelse(turnout$educate > 15, 1, 0)

z.out <- zelig(vote ~ age , data = turnout, model = "ls",
               by = c("race", "dedu"))
summary(z.out)
x.out <- setx(z.out, age = 18)
x1.out <- setx(z.out, age = 20)
set.seed(42)
s.out <- sim(z.out, x.out, x1.out)
summary(s.out)

z5 <- zls$new()
z5$zelig(vote ~ age , data = turnout, by = c("race", "dedu"))
z5$zelig.out
z5
z5$setx(age = 18)
z5$setx1(age=20)
z5$setrange(age = 18:20)
z5$setx.out
z5$sim(10)
z5$sim.out
z5$summarize()

z5 <- zls$new()
z5$zelig(vote ~ age + income , data = turnout)
z5$zelig.out
z5
z5$setx(age = 18)
z5$setx1(age = 20)
z5$setrange(age = 18:20)
z5$setx.out
z5$sim(10)
z5$sim.out
z5$summarize()

by(turnout,  factor(turnout$race),
   function(x) lm(vote ~ age , data = x))

by(turnout,  factor(turnout$race),
   function(x) Zelig::zelig(vote ~ age , data = x, model = "ls"))

by(turnout,  factor(turnout$race),
   function(x) {
     z.out <<- zelig(vote ~ age , data = x, model = "ls")
     x.out <<- setx(z.out, age = 30)
     s.out <<- sim(z.out, x.out, age = 30)
     })
