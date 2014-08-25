
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
    ##  [1,] 20.43
    ##  [2,] 17.33
    ##  [3,] 21.49
    ##  [4,] 22.55
    ##  [5,] 21.59
    ##  [6,] 18.63
    ##  [7,] 18.41
    ##  [8,] 21.22
    ##  [9,] 22.97
    ## [10,] 22.81
    ## 
    ## [[1]][[1]]$pv
    ##           1
    ##  [1,] 20.43
    ##  [2,] 17.33
    ##  [3,] 21.49
    ##  [4,] 22.55
    ##  [5,] 21.59
    ##  [6,] 18.63
    ##  [7,] 18.41
    ##  [8,] 21.22
    ##  [9,] 22.97
    ## [10,] 22.81
    ## 
    ## 
    ## 
    ## [[2]]
    ## [[2]][[1]]
    ## [[2]][[1]]$ev
    ##           1
    ##  [1,] 30.25
    ##  [2,] 25.89
    ##  [3,] 24.97
    ##  [4,] 24.18
    ##  [5,] 24.30
    ##  [6,] 25.25
    ##  [7,] 27.23
    ##  [8,] 24.48
    ##  [9,] 22.23
    ## [10,] 24.05
    ## 
    ## [[2]][[1]]$pv
    ##           1
    ##  [1,] 30.25
    ##  [2,] 25.89
    ##  [3,] 24.97
    ##  [4,] 24.18
    ##  [5,] 24.30
    ##  [6,] 25.25
    ##  [7,] 27.23
    ##  [8,] 24.48
    ##  [9,] 22.23
    ## [10,] 24.05
    ## 
    ## 
    ## 
    ## [[3]]
    ## [[3]][[1]]
    ## [[3]][[1]]$ev
    ##           1
    ##  [1,] 27.95
    ##  [2,] 26.17
    ##  [3,] 28.71
    ##  [4,] 32.53
    ##  [5,] 29.50
    ##  [6,] 33.82
    ##  [7,] 30.98
    ##  [8,] 29.88
    ##  [9,] 27.88
    ## [10,] 28.81
    ## 
    ## [[3]][[1]]$pv
    ##           1
    ##  [1,] 27.95
    ##  [2,] 26.17
    ##  [3,] 28.71
    ##  [4,] 32.53
    ##  [5,] 29.50
    ##  [6,] 33.82
    ##  [7,] 30.98
    ##  [8,] 29.88
    ##  [9,] 27.88
    ## [10,] 28.81
    ## 
    ## 
    ## 
    ## [[4]]
    ## [[4]][[1]]
    ## [[4]][[1]]$ev
    ##           1
    ##  [1,] 32.18
    ##  [2,] 30.96
    ##  [3,] 36.45
    ##  [4,] 29.52
    ##  [5,] 32.44
    ##  [6,] 36.69
    ##  [7,] 34.91
    ##  [8,] 35.49
    ##  [9,] 32.52
    ## [10,] 37.14
    ## 
    ## [[4]][[1]]$pv
    ##           1
    ##  [1,] 32.18
    ##  [2,] 30.96
    ##  [3,] 36.45
    ##  [4,] 29.52
    ##  [5,] 32.44
    ##  [6,] 36.69
    ##  [7,] 34.91
    ##  [8,] 35.49
    ##  [9,] 32.52
    ## [10,] 37.14
    ## 
    ## 
    ## 
    ## [[5]]
    ## [[5]][[1]]
    ## [[5]][[1]]$ev
    ##           1
    ##  [1,] 39.57
    ##  [2,] 39.58
    ##  [3,] 36.89
    ##  [4,] 44.17
    ##  [5,] 36.08
    ##  [6,] 37.34
    ##  [7,] 37.32
    ##  [8,] 40.34
    ##  [9,] 41.03
    ## [10,] 36.96
    ## 
    ## [[5]][[1]]$pv
    ##           1
    ##  [1,] 39.57
    ##  [2,] 39.58
    ##  [3,] 36.89
    ##  [4,] 44.17
    ##  [5,] 36.08
    ##  [6,] 37.34
    ##  [7,] 37.32
    ##  [8,] 40.34
    ##  [9,] 41.03
    ## [10,] 36.96
    ## 
    ## 
    ## 
    ## [[6]]
    ## [[6]][[1]]
    ## [[6]][[1]]$ev
    ##           1
    ##  [1,] 41.52
    ##  [2,] 41.62
    ##  [3,] 37.49
    ##  [4,] 45.32
    ##  [5,] 39.03
    ##  [6,] 38.90
    ##  [7,] 38.84
    ##  [8,] 39.61
    ##  [9,] 40.39
    ## [10,] 36.90
    ## 
    ## [[6]][[1]]$pv
    ##           1
    ##  [1,] 41.52
    ##  [2,] 41.62
    ##  [3,] 37.49
    ##  [4,] 45.32
    ##  [5,] 39.03
    ##  [6,] 38.90
    ##  [7,] 38.84
    ##  [8,] 39.61
    ##  [9,] 40.39
    ## [10,] 36.90
    ## 
    ## 
    ## 
    ## [[7]]
    ## [[7]][[1]]
    ## [[7]][[1]]$ev
    ##           1
    ##  [1,] 45.24
    ##  [2,] 50.19
    ##  [3,] 47.29
    ##  [4,] 45.77
    ##  [5,] 43.37
    ##  [6,] 46.00
    ##  [7,] 41.96
    ##  [8,] 44.40
    ##  [9,] 43.36
    ## [10,] 44.26
    ## 
    ## [[7]][[1]]$pv
    ##           1
    ##  [1,] 45.24
    ##  [2,] 50.19
    ##  [3,] 47.29
    ##  [4,] 45.77
    ##  [5,] 43.37
    ##  [6,] 46.00
    ##  [7,] 41.96
    ##  [8,] 44.40
    ##  [9,] 43.36
    ## [10,] 44.26
    ## 
    ## 
    ## 
    ## [[8]]
    ## [[8]][[1]]
    ## [[8]][[1]]$ev
    ##           1
    ##  [1,] 49.50
    ##  [2,] 49.21
    ##  [3,] 48.06
    ##  [4,] 51.84
    ##  [5,] 51.88
    ##  [6,] 53.32
    ##  [7,] 51.58
    ##  [8,] 48.90
    ##  [9,] 51.61
    ## [10,] 46.72
    ## 
    ## [[8]][[1]]$pv
    ##           1
    ##  [1,] 49.50
    ##  [2,] 49.21
    ##  [3,] 48.06
    ##  [4,] 51.84
    ##  [5,] 51.88
    ##  [6,] 53.32
    ##  [7,] 51.58
    ##  [8,] 48.90
    ##  [9,] 51.61
    ## [10,] 46.72
    ## 
    ## 
    ## 
    ## [[9]]
    ## [[9]][[1]]
    ## [[9]][[1]]$ev
    ##           1
    ##  [1,] 52.76
    ##  [2,] 55.75
    ##  [3,] 56.24
    ##  [4,] 58.88
    ##  [5,] 57.09
    ##  [6,] 54.15
    ##  [7,] 53.97
    ##  [8,] 56.32
    ##  [9,] 53.03
    ## [10,] 52.59
    ## 
    ## [[9]][[1]]$pv
    ##           1
    ##  [1,] 52.76
    ##  [2,] 55.75
    ##  [3,] 56.24
    ##  [4,] 58.88
    ##  [5,] 57.09
    ##  [6,] 54.15
    ##  [7,] 53.97
    ##  [8,] 56.32
    ##  [9,] 53.03
    ## [10,] 52.59
    ## 
    ## 
    ## 
    ## [[10]]
    ## [[10]][[1]]
    ## [[10]][[1]]$ev
    ##           1
    ##  [1,] 55.32
    ##  [2,] 57.70
    ##  [3,] 56.94
    ##  [4,] 62.55
    ##  [5,] 54.90
    ##  [6,] 56.24
    ##  [7,] 58.85
    ##  [8,] 55.25
    ##  [9,] 57.11
    ## [10,] 57.57
    ## 
    ## [[10]][[1]]$pv
    ##           1
    ##  [1,] 55.32
    ##  [2,] 57.70
    ##  [3,] 56.94
    ##  [4,] 62.55
    ##  [5,] 54.90
    ##  [6,] 56.24
    ##  [7,] 58.85
    ##  [8,] 55.25
    ##  [9,] 57.11
    ## [10,] 57.57
    ## 
    ## 
    ## 
    ## [[11]]
    ## [[11]][[1]]
    ## [[11]][[1]]$ev
    ##           1
    ##  [1,] 63.57
    ##  [2,] 61.46
    ##  [3,] 59.21
    ##  [4,] 63.19
    ##  [5,] 60.34
    ##  [6,] 57.97
    ##  [7,] 59.92
    ##  [8,] 62.10
    ##  [9,] 61.20
    ## [10,] 63.28
    ## 
    ## [[11]][[1]]$pv
    ##           1
    ##  [1,] 63.57
    ##  [2,] 61.46
    ##  [3,] 59.21
    ##  [4,] 63.19
    ##  [5,] 60.34
    ##  [6,] 57.97
    ##  [7,] 59.92
    ##  [8,] 62.10
    ##  [9,] 61.20
    ## [10,] 63.28



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



