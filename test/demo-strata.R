library(Zelig)
data(turnout)

z.out <- zelig(vote ~ age , data = turnout, model = "ls",
               by = "race")
summary(z.out)
x.out <- setx(z.out, age = 18)
s.out <- sim(z.out, x.out)
summary(s.out)


r <- split(turnout, factor(turnout$race))
r[[1]]
r[[2]]

z5 <- zls$new()
z5$zelig(vote ~ age , data = turnout, by = "race")
z5$zelig.out
z5$zelig.out.by
z5
z5$setx()
z5$setx1(age=20)
z5$setrange(age = 18:20)
z5$setx.out.by
z5$sim()
z5$summarize()

z5 <- zls$new()
z5$zelig(vote ~ age , data = turnout)
z5$zelig.out
z5$setx(age = 18)
z5$setrange(age = 18:20)
z5$setx.out
z5$setx.out.by
z5$sim()
z5$summarize()

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
