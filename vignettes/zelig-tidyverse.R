## ----setup, include=FALSE------------------------------------------------
knitr::opts_knit$set(
        stop_on_error = 2L
)

## ----message=FALSE-------------------------------------------------------
library(tidyverse)

## ---- echo=FALSE---------------------------------------------------------
swiss <- swiss[, c('Fertility', 'Agriculture', 'Examination')]
head(swiss)

## ----fig.height=5, fig.width=5, message=FALSE----------------------------
library(Zelig)
library(tidyverse)

swiss %>% 
    zelig(Fertility ~ Agriculture + Examination, model = 'ls', data = ., 
          cite = FALSE) %>%
    setx(Agriculture = seq(1, 90, by = 5)) %>%
    sim() %>% 
    plot()

## ----fig.height=11, fig.width=7------------------------------------------
swiss %>% 
    zelig(Fertility ~ Agriculture + Examination, model = 'ls', data = ., 
          cite = FALSE) %>%
    setx(Agriculture = 10) %>%
    setx1(Agriculture = 90) %>%
    sim() %>% 
    plot()

