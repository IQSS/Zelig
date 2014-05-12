source(file.path("..", "R", "utils.R"))
source(file.path("..", "R", "model-zelig.R"))
source(file.path("..", "R", "model-ls.R"))
source(file.path("..", "R", "wrappers.R"))

z5 <- zls$new()
z5$zelig(dist ~ speed, data = cars)
z5

zeligw <- z5$zelig

z.out <- zeligw(dist ~ speed, model = "ls", data = cars)
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
