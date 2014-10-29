z5 <- zls$new()
z5$zelig(dist ~ speed, data = cars)
z5

z.out <- Zelig::zelig(dist ~ speed, model = "ls", data = cars)
print(z.out)
z.out$zelig.call


## As expected
f <- function() {
  z5 <- zls$new()
  z5$zelig(dist ~ speed, data = cars)
  print(z5)
  z5$setx(speed = 3)
  z5$sim()
  z5$summarize()
  return(z5)
}

Z <- f()
print(Z)
Z$summarize()

g <- function() {
  z.out <- Zelig::zelig(dist ~ speed, model = "ls", data = cars)
  print(z5)
  Zelig::setx(z.out, speed = 3)
  Zelig::sim(z.out)
  Zelig::summary(z.out)
  return(z.out)
}

ZZ <- g()
ZZ
g()$summarize()

data(cars)
x <- cars
zelig(dist ~ speed, data = x, model = "ls")


rm(list = ls())

test_function <- function() {
  data(cars)
  x <- cars
  zelig(dist ~ speed, data = x, model = "ls")
}

test_function()

test_function_refclasses <- function() {
  data(cars)
  x <- cars
  zref <- zls$new()
  zref$zelig(dist ~ speed, data = x)
  return(zref)
}

test_function_refclasses()
