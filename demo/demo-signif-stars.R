data(cars)
z.out <- zelig(dist ~ speed, cars, model = "ls")
summary(z.out)
summary(z.out, signif.stars = TRUE)
