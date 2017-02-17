## ----setup, include=FALSE------------------------------------------------
knitr::opts_knit$set(
        stop_on_error = 2L
)

## ------------------------------------------------------------------------
library(tidyverse)

## ---- echo=FALSE---------------------------------------------------------
swiss <- swiss[, c('Fertility', 'Agriculture', 'Examination')]
head(swiss)

