## ----setup, include=FALSE------------------------------------------------
knitr::opts_knit$set(
        stop_on_error = 2L
)

## ------------------------------------------------------------------------
library(Zelig)
library(dplyr)

z.log <- zelig(speed ~ log(dist), data = cars, model = 'ls', cite = FALSE)

## ------------------------------------------------------------------------
setx(z.log, dist = log(26:56)) %>%
    sim() %>%
    ci.plot(xlab = 'dist (log)') 

