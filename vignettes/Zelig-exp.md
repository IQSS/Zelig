---
title: "Zelig-exp"
output:
  html_document:
    theme: null
    css: mystyle.css
    toc: yes
bibliography: zelig5.bib
---

<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Model: Zelig-exp}
-->

# Exponential Regression for Duration Dependent Variables

Use the exponential duration regression model if you have a dependent
variable representing a duration (time until an event). The model
assumes a constant hazard rate for all events. The dependent variable
may be censored (for observations have not yet been completed when data
were collected).

## Syntax


```r
z.out <- zelig(Surv(Y, C) ~ X, model = "exp", data = mydata)
x.out <- setx(z.out)
s.out <- sim(z.out, x = x.out)
```

Exponential models require that the dependent variable be in the form
`Surv(Y, C)`, where `Y` and `C` are
vectors of length $n$. For each observation $i$ in 1, …, $n$, the value
$y_i$ is the duration (lifetime, for example), and the associated $c_i$
is a binary variable such that $c_i = 1$ if the duration is not censored
(`*e.g.*`, the subject dies during the study) or $c_i = 0$ if
the duration is censored (`*e.g.*`, the subject is still
alive at the end of the study and is know to live at least as long as
$y_i$). If $c_i$ is omitted, all Y are assumed to be completed; that is,
time defaults to 1 for all observations.

## Input Values

In addition to the standard inputs, `zelig()` takes the
following additional options for exponential regression:

-   `robust`: defaults to `FALSE`. If
    `TRUE`, `zelig()` computes robust standard
    errors based on sandwich estimators (see @Huber81 and @White80) and
    the options selected in `cluster`.

-   `cluster`: if `robust = TRUE`, you may select
    a variable to define groups of correlated observations. Let
    `x3` be a variable that consists of either discrete
    numeric values, character strings, or factors that define strata.
    Then

        > z.out <- zelig(y ~ x1 + x2, robust = TRUE, cluster = "x3", 
                         model = "exp", data = mydata)

    means that the observations can be correlated within the strata
    defined by the variable `x3`, and that robust standard
    errors should be calculated according to those clusters. If
    `robust = TRUE` but `cluster` is not
    specified, `zelig()` assumes that each observation falls
    into its own cluster.

## Example

Attach the sample data:




```r
data(coalition)
```

Estimate the model:


```r
z.out <- zelig(Surv(duration, ciep12) ~ fract + numst2,
               model = "exp", data = coalition)
```

```
## How to cite this model in Zelig:
##   Olivia Lau, Kosuke Imai, Gary King. 2011.
##   exp: Exponential Regression for Duration Dependent Variables
##   in Kosuke Imai, Gary King, and Olivia Lau, "Zelig: Everyone's Statistical Software,"
##   http://datascience.iq.harvard.edu/zelig
```

View the regression output:









