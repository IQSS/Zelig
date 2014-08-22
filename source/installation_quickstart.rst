
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
    ##  [1,] 18.67
    ##  [2,] 21.54
    ##  [3,] 22.97
    ##  [4,] 19.19
    ##  [5,] 23.58
    ##  [6,] 14.83
    ##  [7,] 24.67
    ##  [8,] 20.85
    ##  [9,] 18.48
    ## [10,] 17.97
    ## 
    ## [[1]][[1]]$pv
    ##           1
    ##  [1,] 18.67
    ##  [2,] 21.54
    ##  [3,] 22.97
    ##  [4,] 19.19
    ##  [5,] 23.58
    ##  [6,] 14.83
    ##  [7,] 24.67
    ##  [8,] 20.85
    ##  [9,] 18.48
    ## [10,] 17.97
    ## 
    ## 
    ## 
    ## [[2]]
    ## [[2]][[1]]
    ## [[2]][[1]]$ev
    ##           1
    ##  [1,] 27.05
    ##  [2,] 26.94
    ##  [3,] 22.71
    ##  [4,] 28.38
    ##  [5,] 27.70
    ##  [6,] 22.21
    ##  [7,] 25.25
    ##  [8,] 29.06
    ##  [9,] 23.77
    ## [10,] 25.61
    ## 
    ## [[2]][[1]]$pv
    ##           1
    ##  [1,] 27.05
    ##  [2,] 26.94
    ##  [3,] 22.71
    ##  [4,] 28.38
    ##  [5,] 27.70
    ##  [6,] 22.21
    ##  [7,] 25.25
    ##  [8,] 29.06
    ##  [9,] 23.77
    ## [10,] 25.61
    ## 
    ## 
    ## 
    ## [[3]]
    ## [[3]][[1]]
    ## [[3]][[1]]$ev
    ##           1
    ##  [1,] 33.67
    ##  [2,] 32.08
    ##  [3,] 29.79
    ##  [4,] 26.37
    ##  [5,] 31.59
    ##  [6,] 34.56
    ##  [7,] 33.28
    ##  [8,] 30.53
    ##  [9,] 32.96
    ## [10,] 29.13
    ## 
    ## [[3]][[1]]$pv
    ##           1
    ##  [1,] 33.67
    ##  [2,] 32.08
    ##  [3,] 29.79
    ##  [4,] 26.37
    ##  [5,] 31.59
    ##  [6,] 34.56
    ##  [7,] 33.28
    ##  [8,] 30.53
    ##  [9,] 32.96
    ## [10,] 29.13
    ## 
    ## 
    ## 
    ## [[4]]
    ## [[4]][[1]]
    ## [[4]][[1]]$ev
    ##           1
    ##  [1,] 29.83
    ##  [2,] 33.39
    ##  [3,] 33.95
    ##  [4,] 33.80
    ##  [5,] 33.01
    ##  [6,] 29.10
    ##  [7,] 33.30
    ##  [8,] 36.07
    ##  [9,] 30.73
    ## [10,] 35.44
    ## 
    ## [[4]][[1]]$pv
    ##           1
    ##  [1,] 29.83
    ##  [2,] 33.39
    ##  [3,] 33.95
    ##  [4,] 33.80
    ##  [5,] 33.01
    ##  [6,] 29.10
    ##  [7,] 33.30
    ##  [8,] 36.07
    ##  [9,] 30.73
    ## [10,] 35.44
    ## 
    ## 
    ## 
    ## [[5]]
    ## [[5]][[1]]
    ## [[5]][[1]]$ev
    ##           1
    ##  [1,] 38.41
    ##  [2,] 35.80
    ##  [3,] 36.37
    ##  [4,] 37.29
    ##  [5,] 39.27
    ##  [6,] 39.50
    ##  [7,] 40.63
    ##  [8,] 39.28
    ##  [9,] 36.32
    ## [10,] 42.76
    ## 
    ## [[5]][[1]]$pv
    ##           1
    ##  [1,] 38.41
    ##  [2,] 35.80
    ##  [3,] 36.37
    ##  [4,] 37.29
    ##  [5,] 39.27
    ##  [6,] 39.50
    ##  [7,] 40.63
    ##  [8,] 39.28
    ##  [9,] 36.32
    ## [10,] 42.76
    ## 
    ## 
    ## 
    ## [[6]]
    ## [[6]][[1]]
    ## [[6]][[1]]$ev
    ##           1
    ##  [1,] 42.01
    ##  [2,] 39.28
    ##  [3,] 37.93
    ##  [4,] 39.10
    ##  [5,] 39.65
    ##  [6,] 39.61
    ##  [7,] 39.73
    ##  [8,] 38.18
    ##  [9,] 45.96
    ## [10,] 42.44
    ## 
    ## [[6]][[1]]$pv
    ##           1
    ##  [1,] 42.01
    ##  [2,] 39.28
    ##  [3,] 37.93
    ##  [4,] 39.10
    ##  [5,] 39.65
    ##  [6,] 39.61
    ##  [7,] 39.73
    ##  [8,] 38.18
    ##  [9,] 45.96
    ## [10,] 42.44
    ## 
    ## 
    ## 
    ## [[7]]
    ## [[7]][[1]]
    ## [[7]][[1]]$ev
    ##           1
    ##  [1,] 48.02
    ##  [2,] 44.90
    ##  [3,] 44.19
    ##  [4,] 46.17
    ##  [5,] 45.26
    ##  [6,] 45.81
    ##  [7,] 44.32
    ##  [8,] 49.33
    ##  [9,] 49.07
    ## [10,] 43.81
    ## 
    ## [[7]][[1]]$pv
    ##           1
    ##  [1,] 48.02
    ##  [2,] 44.90
    ##  [3,] 44.19
    ##  [4,] 46.17
    ##  [5,] 45.26
    ##  [6,] 45.81
    ##  [7,] 44.32
    ##  [8,] 49.33
    ##  [9,] 49.07
    ## [10,] 43.81
    ## 
    ## 
    ## 
    ## [[8]]
    ## [[8]][[1]]
    ## [[8]][[1]]$ev
    ##           1
    ##  [1,] 54.81
    ##  [2,] 46.93
    ##  [3,] 48.92
    ##  [4,] 49.97
    ##  [5,] 48.61
    ##  [6,] 49.48
    ##  [7,] 48.25
    ##  [8,] 48.21
    ##  [9,] 52.28
    ## [10,] 51.02
    ## 
    ## [[8]][[1]]$pv
    ##           1
    ##  [1,] 54.81
    ##  [2,] 46.93
    ##  [3,] 48.92
    ##  [4,] 49.97
    ##  [5,] 48.61
    ##  [6,] 49.48
    ##  [7,] 48.25
    ##  [8,] 48.21
    ##  [9,] 52.28
    ## [10,] 51.02
    ## 
    ## 
    ## 
    ## [[9]]
    ## [[9]][[1]]
    ## [[9]][[1]]$ev
    ##           1
    ##  [1,] 52.77
    ##  [2,] 56.19
    ##  [3,] 54.92
    ##  [4,] 53.46
    ##  [5,] 52.87
    ##  [6,] 52.33
    ##  [7,] 55.70
    ##  [8,] 51.19
    ##  [9,] 58.09
    ## [10,] 53.08
    ## 
    ## [[9]][[1]]$pv
    ##           1
    ##  [1,] 52.77
    ##  [2,] 56.19
    ##  [3,] 54.92
    ##  [4,] 53.46
    ##  [5,] 52.87
    ##  [6,] 52.33
    ##  [7,] 55.70
    ##  [8,] 51.19
    ##  [9,] 58.09
    ## [10,] 53.08
    ## 
    ## 
    ## 
    ## [[10]]
    ## [[10]][[1]]
    ## [[10]][[1]]$ev
    ##           1
    ##  [1,] 59.53
    ##  [2,] 54.60
    ##  [3,] 54.34
    ##  [4,] 58.31
    ##  [5,] 58.35
    ##  [6,] 59.42
    ##  [7,] 53.18
    ##  [8,] 58.67
    ##  [9,] 59.47
    ## [10,] 56.68
    ## 
    ## [[10]][[1]]$pv
    ##           1
    ##  [1,] 59.53
    ##  [2,] 54.60
    ##  [3,] 54.34
    ##  [4,] 58.31
    ##  [5,] 58.35
    ##  [6,] 59.42
    ##  [7,] 53.18
    ##  [8,] 58.67
    ##  [9,] 59.47
    ## [10,] 56.68
    ## 
    ## 
    ## 
    ## [[11]]
    ## [[11]][[1]]
    ## [[11]][[1]]$ev
    ##           1
    ##  [1,] 58.86
    ##  [2,] 63.60
    ##  [3,] 60.98
    ##  [4,] 65.29
    ##  [5,] 61.99
    ##  [6,] 62.72
    ##  [7,] 60.55
    ##  [8,] 64.45
    ##  [9,] 61.69
    ## [10,] 65.37
    ## 
    ## [[11]][[1]]$pv
    ##           1
    ##  [1,] 58.86
    ##  [2,] 63.60
    ##  [3,] 60.98
    ##  [4,] 65.29
    ##  [5,] 61.99
    ##  [6,] 62.72
    ##  [7,] 60.55
    ##  [8,] 64.45
    ##  [9,] 61.69
    ## [10,] 65.37



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



