library(Zelig)

z.out <- zelig(vote ~ age , data = turnout, model = "ls",
               by = "race")
summary(z.out)
x.out <- setx(z.out)
s.out <- sim(z.out, x.out)
summary(s.out)

data(turnout)
r <- split(turnout, factor(turnout$race))
r[[1]]
r[[2]]

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
     s.out <<- setxw(z.out)
     })
