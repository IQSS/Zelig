library(Zelig)
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


r <- split(turnout, factor(turnout$race))
r[[1]]
r[[2]]

z5 <- zls$new()
z5$zelig(vote ~ age , data = turnout, ID = c("race", "dedu"))
z5$zelig.out
z5
z5$setx(age = 18)
z5$setx1(age=20)
z5$setrange(age = 18:20)
z5$setx.out
z5$sim(10)
z5$sim.out
z5$summarize()
.self <- z5

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
.self <- z5
.self$num <- 10

data(turnout)
sturnout <- split(turnout, factor(turnout$race))
z5 <- list()
for (i in seq(sturnout)) {
  z5[[i]] <- zls$new()
  z5[[i]]$zelig(vote ~ age , data = sturnout[[i]])
  z5[[i]]$zelig.out
  z5[[i]]$setx(car = 3)
  z5[[i]]$summarize()
}


z.outw <- zeligw(vote ~ age , data = turnout, model = "ls", by = "race")
x.outw <- setxw(z.outw, age = 18)
x1.outw <- setxw(z.outw, age = 20)
s.outw <- simw(z.outw, x.outw, x1.outw, num = 1000)
summaryw(s.outw)


by(turnout,  factor(turnout$race),
   function(x) lm(vote ~ age , data = x))

by(turnout,  factor(turnout$race),
   function(x) zelig(vote ~ age , data = x, model = "ls"))

z.out <- by(turnout,  factor(turnout$race),
   function(x) zeligw(vote ~ age , data = x, model = "ls"))

by(turnout,  factor(turnout$race),
   function(x) {
     z.out <<- zeligw(vote ~ age , data = x, model = "ls")
     s.out <<- setxw(z.out, age = 30)
     })
