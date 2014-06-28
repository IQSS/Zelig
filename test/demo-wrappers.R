library(MASS)

## Zelig 5
library(Zelig5)
z.out <- zelig(dist ~ speed, model = "ls", data = cars)
print(z.out)
x.out <- setx(z.out, speed=30)
x1.out <- setx(z.out, speed = 50)
s.out <- sim(z.out, x.out, x1.out, num = 1000)
summary(s.out)

## Zelig 4
library(Zelig)
z.out <- Zelig::zelig(dist ~ speed, model = "ls", data = cars)
x.out <- Zelig::setx(z.out, speed=30)
x1.out <- Zelig::setx(z.out, speed = 50)
s.out <- Zelig::sim(z.out, x.out, x1.out, num = 1000)
summary(s.out)
