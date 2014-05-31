z.out <- zelig(vote ~ age , data = turnout, model = "ls",
               by = "race")
summary(z.out)
