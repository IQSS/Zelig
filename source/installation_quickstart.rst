
.. _installation_quickstart:

Installation and Quickstart
=========================

This guide is designed to get you up and running with the current *beta* release of Zelig (5.0.1). 

------------

Installing R and Zelig
~~~~~~~~~~~~~~~~~~~~

Before using Zelig, you will need to download and install both the R statistical program and the Zelig package:

**Installing R**

To install R, go to `http://www.r-project.org/ <http://www.r-project.org/>`_  Select the ``CRAN`` option from the left-hand menu (CRAN is the Comprehensive R Archive Network where all files related to R can be found). Pick a CRAN mirror closest to your current geographic location (there are multiple mirrors of this database in various locations, selecting the one closest to you will be sure to maximize your the speed of your download).  Follow the instructions for downloading R for Linux, Mac OS X, or Windows. 

------------

**Installing Zelig**

Because Zelig 5 is still a *beta* release and is not yet available on ``CRAN`` (with other R software packages), it must be downloaded from Github using the ``devtools`` package.

Once you've successfully installed R, open it, and at the terminal prompt, type in the following commands verbatim:


.. sourcecode:: r
    

    # This installs devtools package, if not already installed
    install.packages("devtools")
    # This loads devtools   	
    library(devtools)
    # This downloads Zelig 5.0.1 from the IQSS Github repo
    install_github('IQSS/Zelig')


If you have successfully installed the program, you will see a the following message: *"DONE (Zelig5)"*.

------------

Quickstart Guide
~~~~~~~~~~~~~~~~
Now that we have successfully downloaded and installed Zelig from Github, we will load the package and walk through am example. The scenario is a simple one: imagine you want to estimate the distance a car has traveled given it's speed and you have a dataset of cars, distance and speed. Throughout the rest of this guide, we will walk you through building a statistical model from this data using Zelig. 


**Loading Zelig**

First, we have to load Zelig into R. After installing both R and
Zelig, open R and type:




.. sourcecode:: r
    

    library(Zelig5)


------------

**Building Models**

Now, lets build a statistical model that captures the relationship a cars distance and speed, where distance is the outcome (dependent) variable and speed is the only explanatory (independent) variable. Given that distance (the DV) is a continuous variable, we should use a least squares regression.

To do so, we must first create Zelig least squares object, then specify our model, and finally regress distance on speed to estimate the relationship between speed and distance:


.. sourcecode:: r
    

    # load toy dataset (when you install R, example datasets are also installed)
    data(cars)
    # initialize Zelig5 least squares object                            
    z5 <- zls$new()  
    # estimate ls model                     
    z5$zelig(dist ~ speed, data = cars)
    # you can now get model summary estimates
    z5


::

    ## 
    ## Call:
    ## stats::lm(formula = dist ~ speed, data = cars)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -29.07  -9.53  -2.27   9.21  43.20 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -17.579      6.758   -2.60    0.012 *  
    ## speed          3.932      0.416    9.46  1.5e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 15.4 on 48 degrees of freedom
    ## Multiple R-squared:  0.651,	Adjusted R-squared:  0.644 
    ## F-statistic: 89.6 on 1 and 48 DF,  p-value: 1.49e-12



So what do our model estimates tell us? First, off we can see that the positive 3.93 estimate for speed suggests a positive relationship between speed and distance. In particular, we would interpret this coefficient as a one unit increase in speed (e.g., mph) leads to a 3 unit increase in distance (e.g., miles). This interpretation is not very intuitive. Perhaps, we want to know how the distance a car can travel changes over a range of speed (e.g., 10 to 20 mph).

Zelig makes this simple, by automating the translation of model estimates in interpretable quantities of interest (more on this below) using Monte Carlo simulations. To get this process started we need to set explanatory variables in our model (i.e., speed) using the ``$setx()`` or ``$setrange()`` method:


.. sourcecode:: r
    

    # simulate over a range of speed between 10 and 20 mph
    z5$setrange(speed = 10:20)
    
    # you can also set covariates at particular value using $setx()
    z5$setx(speed = 30)


Now that we've set our variables, all we have to do is run our simulations:


.. sourcecode:: r
    

    #run 10 simulations and estimate quantities of interest
    z5$sim(num = 10)
    # default is 1,000 simulations


Now we've estimated a model and calculated interpretable estimates across a range of speed (e.g., 10 - 20 mph). What can we do with them? Zelig gives you access to estimated quantities of interest and makes plotting and presenting them particularly easy.

------------

**Quantities of Interest**

As mentioned earlier, a major feature of Zelig is the translation of model estimates into easy to interpret quantities of interest (QIs). These QIs (e.g., expected and predicted values) can be accessed via the ``$sim.out`` field:


.. sourcecode:: r
    

    z5$sim.out$range


::

    ## [[1]]
    ## [[1]][[1]]
    ## [[1]][[1]]$ev
    ##           1
    ##  [1,] 22.64
    ##  [2,] 18.83
    ##  [3,] 14.22
    ##  [4,] 20.99
    ##  [5,] 23.70
    ##  [6,] 20.16
    ##  [7,] 19.67
    ##  [8,] 13.15
    ##  [9,] 24.77
    ## [10,] 15.09
    ## 
    ## [[1]][[1]]$pv
    ##           1
    ##  [1,] 22.64
    ##  [2,] 18.83
    ##  [3,] 14.22
    ##  [4,] 20.99
    ##  [5,] 23.70
    ##  [6,] 20.16
    ##  [7,] 19.67
    ##  [8,] 13.15
    ##  [9,] 24.77
    ## [10,] 15.09
    ## 
    ## 
    ## 
    ## [[2]]
    ## [[2]][[1]]
    ## [[2]][[1]]$ev
    ##           1
    ##  [1,] 19.86
    ##  [2,] 26.12
    ##  [3,] 27.34
    ##  [4,] 22.27
    ##  [5,] 25.18
    ##  [6,] 24.46
    ##  [7,] 28.78
    ##  [8,] 28.62
    ##  [9,] 21.92
    ## [10,] 24.22
    ## 
    ## [[2]][[1]]$pv
    ##           1
    ##  [1,] 19.86
    ##  [2,] 26.12
    ##  [3,] 27.34
    ##  [4,] 22.27
    ##  [5,] 25.18
    ##  [6,] 24.46
    ##  [7,] 28.78
    ##  [8,] 28.62
    ##  [9,] 21.92
    ## [10,] 24.22
    ## 
    ## 
    ## 
    ## [[3]]
    ## [[3]][[1]]
    ## [[3]][[1]]$ev
    ##           1
    ##  [1,] 26.17
    ##  [2,] 34.90
    ##  [3,] 33.31
    ##  [4,] 24.76
    ##  [5,] 31.62
    ##  [6,] 30.85
    ##  [7,] 30.21
    ##  [8,] 26.27
    ##  [9,] 31.51
    ## [10,] 28.93
    ## 
    ## [[3]][[1]]$pv
    ##           1
    ##  [1,] 26.17
    ##  [2,] 34.90
    ##  [3,] 33.31
    ##  [4,] 24.76
    ##  [5,] 31.62
    ##  [6,] 30.85
    ##  [7,] 30.21
    ##  [8,] 26.27
    ##  [9,] 31.51
    ## [10,] 28.93
    ## 
    ## 
    ## 
    ## [[4]]
    ## [[4]][[1]]
    ## [[4]][[1]]$ev
    ##           1
    ##  [1,] 35.17
    ##  [2,] 32.79
    ##  [3,] 33.29
    ##  [4,] 35.36
    ##  [5,] 37.97
    ##  [6,] 31.91
    ##  [7,] 34.44
    ##  [8,] 32.17
    ##  [9,] 32.75
    ## [10,] 27.12
    ## 
    ## [[4]][[1]]$pv
    ##           1
    ##  [1,] 35.17
    ##  [2,] 32.79
    ##  [3,] 33.29
    ##  [4,] 35.36
    ##  [5,] 37.97
    ##  [6,] 31.91
    ##  [7,] 34.44
    ##  [8,] 32.17
    ##  [9,] 32.75
    ## [10,] 27.12
    ## 
    ## 
    ## 
    ## [[5]]
    ## [[5]][[1]]
    ## [[5]][[1]]$ev
    ##           1
    ##  [1,] 35.70
    ##  [2,] 37.24
    ##  [3,] 37.83
    ##  [4,] 41.69
    ##  [5,] 39.28
    ##  [6,] 37.03
    ##  [7,] 35.71
    ##  [8,] 39.94
    ##  [9,] 35.88
    ## [10,] 36.83
    ## 
    ## [[5]][[1]]$pv
    ##           1
    ##  [1,] 35.70
    ##  [2,] 37.24
    ##  [3,] 37.83
    ##  [4,] 41.69
    ##  [5,] 39.28
    ##  [6,] 37.03
    ##  [7,] 35.71
    ##  [8,] 39.94
    ##  [9,] 35.88
    ## [10,] 36.83
    ## 
    ## 
    ## 
    ## [[6]]
    ## [[6]][[1]]
    ## [[6]][[1]]$ev
    ##           1
    ##  [1,] 41.85
    ##  [2,] 39.78
    ##  [3,] 40.46
    ##  [4,] 37.90
    ##  [5,] 40.26
    ##  [6,] 44.92
    ##  [7,] 41.38
    ##  [8,] 44.00
    ##  [9,] 42.13
    ## [10,] 42.81
    ## 
    ## [[6]][[1]]$pv
    ##           1
    ##  [1,] 41.85
    ##  [2,] 39.78
    ##  [3,] 40.46
    ##  [4,] 37.90
    ##  [5,] 40.26
    ##  [6,] 44.92
    ##  [7,] 41.38
    ##  [8,] 44.00
    ##  [9,] 42.13
    ## [10,] 42.81
    ## 
    ## 
    ## 
    ## [[7]]
    ## [[7]][[1]]
    ## [[7]][[1]]$ev
    ##           1
    ##  [1,] 45.62
    ##  [2,] 46.57
    ##  [3,] 44.60
    ##  [4,] 45.79
    ##  [5,] 45.04
    ##  [6,] 43.39
    ##  [7,] 46.32
    ##  [8,] 44.07
    ##  [9,] 44.48
    ## [10,] 47.82
    ## 
    ## [[7]][[1]]$pv
    ##           1
    ##  [1,] 45.62
    ##  [2,] 46.57
    ##  [3,] 44.60
    ##  [4,] 45.79
    ##  [5,] 45.04
    ##  [6,] 43.39
    ##  [7,] 46.32
    ##  [8,] 44.07
    ##  [9,] 44.48
    ## [10,] 47.82
    ## 
    ## 
    ## 
    ## [[8]]
    ## [[8]][[1]]
    ## [[8]][[1]]$ev
    ##           1
    ##  [1,] 48.95
    ##  [2,] 47.42
    ##  [3,] 52.20
    ##  [4,] 48.28
    ##  [5,] 51.69
    ##  [6,] 43.78
    ##  [7,] 50.31
    ##  [8,] 47.31
    ##  [9,] 48.38
    ## [10,] 48.45
    ## 
    ## [[8]][[1]]$pv
    ##           1
    ##  [1,] 48.95
    ##  [2,] 47.42
    ##  [3,] 52.20
    ##  [4,] 48.28
    ##  [5,] 51.69
    ##  [6,] 43.78
    ##  [7,] 50.31
    ##  [8,] 47.31
    ##  [9,] 48.38
    ## [10,] 48.45
    ## 
    ## 
    ## 
    ## [[9]]
    ## [[9]][[1]]
    ## [[9]][[1]]$ev
    ##           1
    ##  [1,] 50.04
    ##  [2,] 54.90
    ##  [3,] 53.53
    ##  [4,] 52.40
    ##  [5,] 50.17
    ##  [6,] 54.94
    ##  [7,] 51.90
    ##  [8,] 55.33
    ##  [9,] 50.33
    ## [10,] 53.98
    ## 
    ## [[9]][[1]]$pv
    ##           1
    ##  [1,] 50.04
    ##  [2,] 54.90
    ##  [3,] 53.53
    ##  [4,] 52.40
    ##  [5,] 50.17
    ##  [6,] 54.94
    ##  [7,] 51.90
    ##  [8,] 55.33
    ##  [9,] 50.33
    ## [10,] 53.98
    ## 
    ## 
    ## 
    ## [[10]]
    ## [[10]][[1]]
    ## [[10]][[1]]$ev
    ##           1
    ##  [1,] 52.06
    ##  [2,] 55.08
    ##  [3,] 61.78
    ##  [4,] 59.29
    ##  [5,] 60.03
    ##  [6,] 57.10
    ##  [7,] 62.01
    ##  [8,] 55.90
    ##  [9,] 64.34
    ## [10,] 62.63
    ## 
    ## [[10]][[1]]$pv
    ##           1
    ##  [1,] 52.06
    ##  [2,] 55.08
    ##  [3,] 61.78
    ##  [4,] 59.29
    ##  [5,] 60.03
    ##  [6,] 57.10
    ##  [7,] 62.01
    ##  [8,] 55.90
    ##  [9,] 64.34
    ## [10,] 62.63
    ## 
    ## 
    ## 
    ## [[11]]
    ## [[11]][[1]]
    ## [[11]][[1]]$ev
    ##           1
    ##  [1,] 57.50
    ##  [2,] 62.96
    ##  [3,] 63.03
    ##  [4,] 60.74
    ##  [5,] 60.39
    ##  [6,] 61.62
    ##  [7,] 61.25
    ##  [8,] 56.19
    ##  [9,] 60.61
    ## [10,] 61.66
    ## 
    ## [[11]][[1]]$pv
    ##           1
    ##  [1,] 57.50
    ##  [2,] 62.96
    ##  [3,] 63.03
    ##  [4,] 60.74
    ##  [5,] 60.39
    ##  [6,] 61.62
    ##  [7,] 61.25
    ##  [8,] 56.19
    ##  [9,] 60.61
    ## [10,] 61.66



------------

**Plots**

A second major Zelig feature is how easy it is to plot QIs for presentation in slides or an artcle. Using the ``plot()`` function on the ``z5$s.out`` will produce ready-to-use plots with labels and confidence intervals.

*Plots of QI's from binary choice model:*  


.. sourcecode:: r
    

    z5$graph()


::

    ## Error: 'graph' is not a valid field or method name for reference class
    ## "Zelig-ls"



------------

*Plot of expected values across range of simulations:*



