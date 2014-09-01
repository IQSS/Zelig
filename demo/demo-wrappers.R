## Zelig 5
library(Zelig)
z.out5 <- zelig(dist ~ speed, model = "ls", data = cars)
print(z.out5)
summary(z.out5)
x.out5 <- Zelig::setx(z.out5, speed=30)
x1.out5 <- Zelig::setx(z.out5, speed = 50)
s.out5 <- Zelig::sim(z.out5, x.out5, x1.out5, num = 1000)
print(s.out5)
summary(s.out5)

## Zelig 4
library(Zelig4)
z.out4 <- Zelig4::zelig(dist ~ speed, model = "ls", data = cars)
x.out4 <- Zelig4::setx(z.out4, speed = 30)
x1.out4 <- Zelig4::setx(z.out4, speed = 50)
s.out4 <- Zelig4::sim(z.out4, x.out4, x1.out4, num = 1000)
summary(s.out4)
