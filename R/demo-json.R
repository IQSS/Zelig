library(MASS)
library(jsonlite)
data(cars)

source(file.path("..", "R", "utils.R"))
source(file.path("..", "R", "model-zelig.R"))
source(file.path("..", "R", "model-ls.R"))

z5 <- zls$new()
z5$zelig(dist ~ speed, data = cars)
z5
z5$setx(speed = 30)
set.seed(42)
z5$sim(num=100)
z5$summarize()

z5$toJSON()
cat(z5$json)
