data(turnout)
z.out1 <- zelig(vote ~ race + educate + age, model = "logit",
                data = turnout)
z.out2 <- zelig(vote ~ race + educate, model = "logit", data = turnout)
rocplot(z.out1, z.out2)
