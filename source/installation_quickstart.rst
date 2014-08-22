
.. _installation_quickstart:

Installation & Quickstart
=========================

This guide is designed to get you up and running with the current *beta* release of Zelig (5.0.1). 

------------

Installing R & Zelig
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
    ##  [1,] 21.60
    ##  [2,] 25.69
    ##  [3,] 22.12
    ##  [4,] 21.02
    ##  [5,] 19.72
    ##  [6,] 24.80
    ##  [7,] 22.01
    ##  [8,] 21.28
    ##  [9,] 25.37
    ## [10,] 23.53
    ## 
    ## [[1]][[1]]$pv
    ##           1
    ##  [1,] 21.60
    ##  [2,] 25.69
    ##  [3,] 22.12
    ##  [4,] 21.02
    ##  [5,] 19.72
    ##  [6,] 24.80
    ##  [7,] 22.01
    ##  [8,] 21.28
    ##  [9,] 25.37
    ## [10,] 23.53
    ## 
    ## 
    ## 
    ## [[2]]
    ## [[2]][[1]]
    ## [[2]][[1]]$ev
    ##           1
    ##  [1,] 22.24
    ##  [2,] 23.68
    ##  [3,] 25.06
    ##  [4,] 26.46
    ##  [5,] 29.06
    ##  [6,] 27.54
    ##  [7,] 26.91
    ##  [8,] 29.43
    ##  [9,] 24.06
    ## [10,] 29.91
    ## 
    ## [[2]][[1]]$pv
    ##           1
    ##  [1,] 22.24
    ##  [2,] 23.68
    ##  [3,] 25.06
    ##  [4,] 26.46
    ##  [5,] 29.06
    ##  [6,] 27.54
    ##  [7,] 26.91
    ##  [8,] 29.43
    ##  [9,] 24.06
    ## [10,] 29.91
    ## 
    ## 
    ## 
    ## [[3]]
    ## [[3]][[1]]
    ## [[3]][[1]]$ev
    ##           1
    ##  [1,] 33.28
    ##  [2,] 27.70
    ##  [3,] 30.11
    ##  [4,] 32.15
    ##  [5,] 28.47
    ##  [6,] 34.69
    ##  [7,] 31.31
    ##  [8,] 25.21
    ##  [9,] 24.81
    ## [10,] 29.49
    ## 
    ## [[3]][[1]]$pv
    ##           1
    ##  [1,] 33.28
    ##  [2,] 27.70
    ##  [3,] 30.11
    ##  [4,] 32.15
    ##  [5,] 28.47
    ##  [6,] 34.69
    ##  [7,] 31.31
    ##  [8,] 25.21
    ##  [9,] 24.81
    ## [10,] 29.49
    ## 
    ## 
    ## 
    ## [[4]]
    ## [[4]][[1]]
    ## [[4]][[1]]$ev
    ##           1
    ##  [1,] 31.73
    ##  [2,] 33.37
    ##  [3,] 34.32
    ##  [4,] 25.88
    ##  [5,] 33.28
    ##  [6,] 37.14
    ##  [7,] 35.66
    ##  [8,] 34.06
    ##  [9,] 38.59
    ## [10,] 33.30
    ## 
    ## [[4]][[1]]$pv
    ##           1
    ##  [1,] 31.73
    ##  [2,] 33.37
    ##  [3,] 34.32
    ##  [4,] 25.88
    ##  [5,] 33.28
    ##  [6,] 37.14
    ##  [7,] 35.66
    ##  [8,] 34.06
    ##  [9,] 38.59
    ## [10,] 33.30
    ## 
    ## 
    ## 
    ## [[5]]
    ## [[5]][[1]]
    ## [[5]][[1]]$ev
    ##           1
    ##  [1,] 37.18
    ##  [2,] 41.22
    ##  [3,] 38.68
    ##  [4,] 35.86
    ##  [5,] 38.84
    ##  [6,] 38.79
    ##  [7,] 40.19
    ##  [8,] 35.44
    ##  [9,] 36.42
    ## [10,] 39.09
    ## 
    ## [[5]][[1]]$pv
    ##           1
    ##  [1,] 37.18
    ##  [2,] 41.22
    ##  [3,] 38.68
    ##  [4,] 35.86
    ##  [5,] 38.84
    ##  [6,] 38.79
    ##  [7,] 40.19
    ##  [8,] 35.44
    ##  [9,] 36.42
    ## [10,] 39.09
    ## 
    ## 
    ## 
    ## [[6]]
    ## [[6]][[1]]
    ## [[6]][[1]]$ev
    ##           1
    ##  [1,] 43.66
    ##  [2,] 44.87
    ##  [3,] 38.43
    ##  [4,] 42.09
    ##  [5,] 44.15
    ##  [6,] 42.38
    ##  [7,] 37.49
    ##  [8,] 39.83
    ##  [9,] 41.47
    ## [10,] 43.28
    ## 
    ## [[6]][[1]]$pv
    ##           1
    ##  [1,] 43.66
    ##  [2,] 44.87
    ##  [3,] 38.43
    ##  [4,] 42.09
    ##  [5,] 44.15
    ##  [6,] 42.38
    ##  [7,] 37.49
    ##  [8,] 39.83
    ##  [9,] 41.47
    ## [10,] 43.28
    ## 
    ## 
    ## 
    ## [[7]]
    ## [[7]][[1]]
    ## [[7]][[1]]$ev
    ##           1
    ##  [1,] 45.28
    ##  [2,] 49.06
    ##  [3,] 43.17
    ##  [4,] 42.47
    ##  [5,] 45.22
    ##  [6,] 44.94
    ##  [7,] 46.64
    ##  [8,] 45.19
    ##  [9,] 42.19
    ## [10,] 43.07
    ## 
    ## [[7]][[1]]$pv
    ##           1
    ##  [1,] 45.28
    ##  [2,] 49.06
    ##  [3,] 43.17
    ##  [4,] 42.47
    ##  [5,] 45.22
    ##  [6,] 44.94
    ##  [7,] 46.64
    ##  [8,] 45.19
    ##  [9,] 42.19
    ## [10,] 43.07
    ## 
    ## 
    ## 
    ## [[8]]
    ## [[8]][[1]]
    ## [[8]][[1]]$ev
    ##           1
    ##  [1,] 48.42
    ##  [2,] 48.27
    ##  [3,] 45.94
    ##  [4,] 52.06
    ##  [5,] 45.41
    ##  [6,] 50.42
    ##  [7,] 47.82
    ##  [8,] 49.30
    ##  [9,] 48.15
    ## [10,] 50.12
    ## 
    ## [[8]][[1]]$pv
    ##           1
    ##  [1,] 48.42
    ##  [2,] 48.27
    ##  [3,] 45.94
    ##  [4,] 52.06
    ##  [5,] 45.41
    ##  [6,] 50.42
    ##  [7,] 47.82
    ##  [8,] 49.30
    ##  [9,] 48.15
    ## [10,] 50.12
    ## 
    ## 
    ## 
    ## [[9]]
    ## [[9]][[1]]
    ## [[9]][[1]]$ev
    ##           1
    ##  [1,] 52.45
    ##  [2,] 50.93
    ##  [3,] 52.61
    ##  [4,] 52.88
    ##  [5,] 52.03
    ##  [6,] 57.80
    ##  [7,] 51.63
    ##  [8,] 57.04
    ##  [9,] 52.13
    ## [10,] 52.90
    ## 
    ## [[9]][[1]]$pv
    ##           1
    ##  [1,] 52.45
    ##  [2,] 50.93
    ##  [3,] 52.61
    ##  [4,] 52.88
    ##  [5,] 52.03
    ##  [6,] 57.80
    ##  [7,] 51.63
    ##  [8,] 57.04
    ##  [9,] 52.13
    ## [10,] 52.90
    ## 
    ## 
    ## 
    ## [[10]]
    ## [[10]][[1]]
    ## [[10]][[1]]$ev
    ##           1
    ##  [1,] 57.59
    ##  [2,] 58.49
    ##  [3,] 56.31
    ##  [4,] 56.95
    ##  [5,] 56.76
    ##  [6,] 55.61
    ##  [7,] 61.49
    ##  [8,] 55.52
    ##  [9,] 59.77
    ## [10,] 56.55
    ## 
    ## [[10]][[1]]$pv
    ##           1
    ##  [1,] 57.59
    ##  [2,] 58.49
    ##  [3,] 56.31
    ##  [4,] 56.95
    ##  [5,] 56.76
    ##  [6,] 55.61
    ##  [7,] 61.49
    ##  [8,] 55.52
    ##  [9,] 59.77
    ## [10,] 56.55
    ## 
    ## 
    ## 
    ## [[11]]
    ## [[11]][[1]]
    ## [[11]][[1]]$ev
    ##           1
    ##  [1,] 62.63
    ##  [2,] 62.63
    ##  [3,] 63.40
    ##  [4,] 61.15
    ##  [5,] 58.92
    ##  [6,] 61.14
    ##  [7,] 62.26
    ##  [8,] 59.30
    ##  [9,] 62.18
    ## [10,] 60.82
    ## 
    ## [[11]][[1]]$pv
    ##           1
    ##  [1,] 62.63
    ##  [2,] 62.63
    ##  [3,] 63.40
    ##  [4,] 61.15
    ##  [5,] 58.92
    ##  [6,] 61.14
    ##  [7,] 62.26
    ##  [8,] 59.30
    ##  [9,] 62.18
    ## [10,] 60.82



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



