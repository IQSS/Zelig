## ----setup, include=FALSE------------------------------------------------
knitr::opts_knit$set(
  stop_on_error = 2L
)

## ----eval=FALSE----------------------------------------------------------
#  data(turnout)
#  z5 <- zlogit$new()
#  z5$zelig(vote ~ age, data = turnout)
#  z5$setx()
#  z5$sim()
#  z5$graph()

