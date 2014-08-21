Zelig-ls
~~~~~~~~~~~~~~~~

Least Squares Regression for Continuous Dependent Variables

Use least squares regression analysis to estimate the best linear
predictor for the specified dependent variables.

Syntax
______



.. sourcecode:: r
    

    z.out <- zelig(Y ~ X1 + X2, model = "ls", data = mydata)
    x.out <- setx(z.out)
    s.out <- sim(z.out, x = x.out)



Examples
______

Basic Example with First Differences
+++++

Attach sample data:


.. sourcecode:: r
    

    library(Zelig5)


::

    ## Loading required package: methods
    ## Loading required package: MASS
    ## Loading required package: survival
    ## Loading required package: splines
    ## Loading required package: VGAM
    ## Loading required package: stats4
    ## Loading required package: jsonlite
    ## 
    ## Attaching package: 'jsonlite'
    ## 
    ## The following object is masked from 'package:utils':
    ## 
    ##     View
    ## 
    ## Loading required package: AER
    ## Loading required package: car
    ## 
    ## Attaching package: 'car'
    ## 
    ## The following object is masked from 'package:VGAM':
    ## 
    ##     logit
    ## 
    ## Loading required package: lmtest
    ## Loading required package: zoo
    ## 
    ## Attaching package: 'zoo'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric
    ## 
    ## 
    ## Attaching package: 'lmtest'
    ## 
    ## The following object is masked from 'package:VGAM':
    ## 
    ##     lrtest
    ## 
    ## Loading required package: sandwich
    ## 
    ## Attaching package: 'AER'
    ## 
    ## The following object is masked from 'package:VGAM':
    ## 
    ##     tobit
    ## 
    ## Loading required package: plyr
    ## Loading required package: dplyr
    ## 
    ## Attaching package: 'dplyr'
    ## 
    ## The following objects are masked from 'package:plyr':
    ## 
    ##     arrange, desc, failwith, id, mutate, summarise, summarize
    ## 
    ## The following object is masked from 'package:MASS':
    ## 
    ##     select
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union



::

    ## Warning: replacing previous import by 'VGAM::show' when loading 'Zelig5'
    ## Warning: replacing previous import by 'AER::tobit' when loading 'Zelig5'
    ## Warning: replacing previous import by 'quantreg::untangle.specials' when loading 'Zelig5'
    ## Warning: replacing previous import by 'dplyr::arrange' when loading 'Zelig5'
    ## Warning: replacing previous import by 'dplyr::desc' when loading 'Zelig5'
    ## Warning: replacing previous import by 'dplyr::failwith' when loading 'Zelig5'
    ## Warning: replacing previous import by 'dplyr::id' when loading 'Zelig5'
    ## Warning: replacing previous import by 'dplyr::mutate' when loading 'Zelig5'
    ## Warning: replacing previous import by 'dplyr::select' when loading 'Zelig5'
    ## Warning: replacing previous import by 'dplyr::summarise' when loading 'Zelig5'
    ## Warning: replacing previous import by 'dplyr::summarize' when loading 'Zelig5'
    ## Warning: replacing previous import by 'methods::setRefClass' when loading 'Zelig5'


.. sourcecode:: r
    

    data(macro)


Estimate model:


.. sourcecode:: r
    

    z.out1 <- zelig(unem ~ gdp + capmob + trade, model = "ls", data = macro)


::

    ## How to cite this model in Zelig:
    ##   Kosuke Imai, Gary King, and Olivia Lau. 2007.
    ##   ls: Least Squares Regression for Continuous Dependent Variables
    ##   in Kosuke Imai, Gary King, and Olivia Lau, "Zelig: Everyone's Statistical Software,"
    ##   http://datascience.iq.harvard.edu/zelig



Summarize regression coefficients:


.. sourcecode:: r
    

    summary(z.out1)


::

    ## Model: 1
    ## Call:
    ## stats::lm(formula = unem ~ gdp + capmob + trade, data = .)
    ## 
    ## Coefficients:
    ## (Intercept)          gdp       capmob        trade  
    ##      6.1813      -0.3236       1.4219       0.0199  
    ## 
    ## Next step: Use 'setx' method



Set explanatory variables to their default (mean/mode) values, with high (80th percentile) and low (20th percentile) values for the trade variable:


.. sourcecode:: r
    

    x.high <- setx(z.out1, trade = quantile(macro$trade, 0.8))
    x.low <- setx(z.out1, trade = quantile(macro$trade, 0.2))


Generate first differences for the effect of high versus low trade on GDP:


.. sourcecode:: r
    

    s.out1 <- sim(z.out1, x = x.high, x1 = x.low)


Using Dummy Variables
+++++

Estimate a model with fixed effects for each country (see for help
with dummy variables). Note that you do not need to create dummy
variables, as the program will automatically parse the unique values
in the selected variable into discrete levels.


.. sourcecode:: r
    

    z.out2 <- zelig(unem ~ gdp + trade + capmob + country,
                    model = "ls", data = macro)


::

    ## How to cite this model in Zelig:
    ##   Kosuke Imai, Gary King, and Olivia Lau. 2007.
    ##   ls: Least Squares Regression for Continuous Dependent Variables
    ##   in Kosuke Imai, Gary King, and Olivia Lau, "Zelig: Everyone's Statistical Software,"
    ##   http://datascience.iq.harvard.edu/zelig




.. sourcecode:: r
    

    z.out3 <- zelig(unem ~ gdp + trade + capmob + as.factor(country),
                    model = "ls", data = macro)


::

    ## How to cite this model in Zelig:
    ##   Kosuke Imai, Gary King, and Olivia Lau. 2007.
    ##   ls: Least Squares Regression for Continuous Dependent Variables
    ##   in Kosuke Imai, Gary King, and Olivia Lau, "Zelig: Everyone's Statistical Software,"
    ##   http://datascience.iq.harvard.edu/zelig




.. sourcecode:: r
    

    x.US <- setx(z.out3, country = "United States")


Set values for the explanatory variables, using the default
mean/mode values, with country set to the United States and Japan,
respectively:



.. sourcecode:: r
    

    x.US <- setx(z.out2, country = "United States")
    x.Japan <- setx(z.out2, country = "Japan")


Simulate quantities of interest:


.. sourcecode:: r
    

    s.out2 <- sim(z.out2, x.US, x.Japan)



.. sourcecode:: r
    

    plot(s.out2)

.. figure:: figure/unnamed-chunk-12.png
    :alt: plot of chunk unnamed-chunk-12

    plot of chunk unnamed-chunk-12

Model
____

-   The *stochastic component* is described by a density with mean
    $\mu_i$ and the common variance :math:`\sigma^2`

.. math::
Y_i \; \sim \; f(y_i \mid \mu_i, \sigma^2).

-   The *systematic component* models the conditional mean as

    $$\mu_i =  x_i \beta$$

    where $x_i$ is the vector of covariates, and $\beta$ is the vector
    of coefficients.

    The least squares estimator is the best linear predictor of a
    dependent variable given $x_i$, and minimizes the sum of squared
    residuals, $\sum_{i=1}^n (Y_i-x_i \beta)^2$.

Quantities of Interest
_____

-   The expected value (`qi$ev`) is the mean of simulations
    from the stochastic component,

    $$E(Y) = x_i \beta,$$

    given a draw of $\beta$ from its sampling distribution.

-   In conditional prediction models, the average expected treatment
    effect (`att.ev`) for the treatment group is

    $$\frac{1}{\sum_{i=1}^n t_i}\sum_{i:t_i=1}^n \left\{ Y_i(t_i=1) -
          E[Y_i(t_i=0)] \right\},$$

    where $t_i$ is a binary explanatory variable defining the treatment
    ($t_i=1$) and control ($t_i=0$) groups. Variation in the simulations
    are due to uncertainty in simulating $E[Y_i(t_i=0)]$, the
    counterfactual expected value of $Y_i$ for observations in the
    treatment group, under the assumption that everything stays the same
    except that the treatment indicator is switched to $t_i=0$.

Output Values
_____

The output of each Zelig command contains useful information which you
may view. For example, if you run
`z.out <- zelig(y ~ x, model = ls, data)`, then you may examine the
available information in `z.out` by using `names(z.out)`, see the
`coefficients` by using `z.out$coefficients`, and
a default summary of information through `summary(z.out)`. Other
elements available through the `$` operator are listed
below.

See also
_____

The least squares regression is part of the stats package by William N.
Venables and Brian D. Ripley [@VenRip02]. In addition, advanced users may
wish to refer to `help(lm)` and `help(lm.fit)`. Robust standard errors
are implemented via the sandwich package by Achim Zeileis
[@Zeileis04]. Sample data are from @KinTomWit00.

