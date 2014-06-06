z.out <- zelig(vote ~ age , data = turnout, model = "ls",
               by = "race")
summary(z.out)

data(turnout)
r <- split(turnout, factor(turnout$race))

r[[1]]
r[[2]]

by(turnout,  factor(turnout$race),
   function(x) lm(vote ~ age , data = x))

