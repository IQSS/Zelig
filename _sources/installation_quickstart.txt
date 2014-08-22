
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
    ##  [1,] 17.20
    ##  [2,] 21.68
    ##  [3,] 19.14
    ##  [4,] 27.76
    ##  [5,] 26.49
    ##  [6,] 23.24
    ##  [7,] 22.47
    ##  [8,] 19.98
    ##  [9,] 26.79
    ## [10,] 21.14
    ## 
    ## [[1]][[1]]$pv
    ##           1
    ##  [1,] 17.20
    ##  [2,] 21.68
    ##  [3,] 19.14
    ##  [4,] 27.76
    ##  [5,] 26.49
    ##  [6,] 23.24
    ##  [7,] 22.47
    ##  [8,] 19.98
    ##  [9,] 26.79
    ## [10,] 21.14
    ## 
    ## 
    ## 
    ## [[2]]
    ## [[2]][[1]]
    ## [[2]][[1]]$ev
    ##           1
    ##  [1,] 23.56
    ##  [2,] 24.78
    ##  [3,] 27.56
    ##  [4,] 24.94
    ##  [5,] 28.83
    ##  [6,] 25.70
    ##  [7,] 21.22
    ##  [8,] 23.90
    ##  [9,] 24.55
    ## [10,] 24.59
    ## 
    ## [[2]][[1]]$pv
    ##           1
    ##  [1,] 23.56
    ##  [2,] 24.78
    ##  [3,] 27.56
    ##  [4,] 24.94
    ##  [5,] 28.83
    ##  [6,] 25.70
    ##  [7,] 21.22
    ##  [8,] 23.90
    ##  [9,] 24.55
    ## [10,] 24.59
    ## 
    ## 
    ## 
    ## [[3]]
    ## [[3]][[1]]
    ## [[3]][[1]]$ev
    ##           1
    ##  [1,] 27.27
    ##  [2,] 33.06
    ##  [3,] 29.48
    ##  [4,] 33.83
    ##  [5,] 30.48
    ##  [6,] 28.97
    ##  [7,] 28.41
    ##  [8,] 29.40
    ##  [9,] 26.19
    ## [10,] 27.65
    ## 
    ## [[3]][[1]]$pv
    ##           1
    ##  [1,] 27.27
    ##  [2,] 33.06
    ##  [3,] 29.48
    ##  [4,] 33.83
    ##  [5,] 30.48
    ##  [6,] 28.97
    ##  [7,] 28.41
    ##  [8,] 29.40
    ##  [9,] 26.19
    ## [10,] 27.65
    ## 
    ## 
    ## 
    ## [[4]]
    ## [[4]][[1]]
    ## [[4]][[1]]$ev
    ##           1
    ##  [1,] 31.90
    ##  [2,] 37.01
    ##  [3,] 31.95
    ##  [4,] 36.58
    ##  [5,] 39.22
    ##  [6,] 37.17
    ##  [7,] 33.71
    ##  [8,] 33.23
    ##  [9,] 33.92
    ## [10,] 30.62
    ## 
    ## [[4]][[1]]$pv
    ##           1
    ##  [1,] 31.90
    ##  [2,] 37.01
    ##  [3,] 31.95
    ##  [4,] 36.58
    ##  [5,] 39.22
    ##  [6,] 37.17
    ##  [7,] 33.71
    ##  [8,] 33.23
    ##  [9,] 33.92
    ## [10,] 30.62
    ## 
    ## 
    ## 
    ## [[5]]
    ## [[5]][[1]]
    ## [[5]][[1]]$ev
    ##           1
    ##  [1,] 38.34
    ##  [2,] 36.77
    ##  [3,] 36.92
    ##  [4,] 38.44
    ##  [5,] 36.58
    ##  [6,] 37.19
    ##  [7,] 37.17
    ##  [8,] 38.18
    ##  [9,] 38.37
    ## [10,] 38.81
    ## 
    ## [[5]][[1]]$pv
    ##           1
    ##  [1,] 38.34
    ##  [2,] 36.77
    ##  [3,] 36.92
    ##  [4,] 38.44
    ##  [5,] 36.58
    ##  [6,] 37.19
    ##  [7,] 37.17
    ##  [8,] 38.18
    ##  [9,] 38.37
    ## [10,] 38.81
    ## 
    ## 
    ## 
    ## [[6]]
    ## [[6]][[1]]
    ## [[6]][[1]]$ev
    ##           1
    ##  [1,] 44.51
    ##  [2,] 38.39
    ##  [3,] 42.16
    ##  [4,] 42.08
    ##  [5,] 38.39
    ##  [6,] 40.91
    ##  [7,] 39.67
    ##  [8,] 41.08
    ##  [9,] 45.52
    ## [10,] 39.69
    ## 
    ## [[6]][[1]]$pv
    ##           1
    ##  [1,] 44.51
    ##  [2,] 38.39
    ##  [3,] 42.16
    ##  [4,] 42.08
    ##  [5,] 38.39
    ##  [6,] 40.91
    ##  [7,] 39.67
    ##  [8,] 41.08
    ##  [9,] 45.52
    ## [10,] 39.69
    ## 
    ## 
    ## 
    ## [[7]]
    ## [[7]][[1]]
    ## [[7]][[1]]$ev
    ##           1
    ##  [1,] 46.70
    ##  [2,] 47.84
    ##  [3,] 45.03
    ##  [4,] 46.68
    ##  [5,] 45.45
    ##  [6,] 46.23
    ##  [7,] 42.29
    ##  [8,] 44.71
    ##  [9,] 40.90
    ## [10,] 45.73
    ## 
    ## [[7]][[1]]$pv
    ##           1
    ##  [1,] 46.70
    ##  [2,] 47.84
    ##  [3,] 45.03
    ##  [4,] 46.68
    ##  [5,] 45.45
    ##  [6,] 46.23
    ##  [7,] 42.29
    ##  [8,] 44.71
    ##  [9,] 40.90
    ## [10,] 45.73
    ## 
    ## 
    ## 
    ## [[8]]
    ## [[8]][[1]]
    ## [[8]][[1]]$ev
    ##           1
    ##  [1,] 46.90
    ##  [2,] 52.94
    ##  [3,] 51.06
    ##  [4,] 48.53
    ##  [5,] 49.47
    ##  [6,] 47.85
    ##  [7,] 53.14
    ##  [8,] 47.67
    ##  [9,] 53.53
    ## [10,] 46.66
    ## 
    ## [[8]][[1]]$pv
    ##           1
    ##  [1,] 46.90
    ##  [2,] 52.94
    ##  [3,] 51.06
    ##  [4,] 48.53
    ##  [5,] 49.47
    ##  [6,] 47.85
    ##  [7,] 53.14
    ##  [8,] 47.67
    ##  [9,] 53.53
    ## [10,] 46.66
    ## 
    ## 
    ## 
    ## [[9]]
    ## [[9]][[1]]
    ## [[9]][[1]]$ev
    ##           1
    ##  [1,] 53.05
    ##  [2,] 48.69
    ##  [3,] 50.82
    ##  [4,] 50.50
    ##  [5,] 53.19
    ##  [6,] 54.85
    ##  [7,] 49.67
    ##  [8,] 51.21
    ##  [9,] 53.17
    ## [10,] 54.49
    ## 
    ## [[9]][[1]]$pv
    ##           1
    ##  [1,] 53.05
    ##  [2,] 48.69
    ##  [3,] 50.82
    ##  [4,] 50.50
    ##  [5,] 53.19
    ##  [6,] 54.85
    ##  [7,] 49.67
    ##  [8,] 51.21
    ##  [9,] 53.17
    ## [10,] 54.49
    ## 
    ## 
    ## 
    ## [[10]]
    ## [[10]][[1]]
    ## [[10]][[1]]$ev
    ##           1
    ##  [1,] 55.18
    ##  [2,] 56.62
    ##  [3,] 54.34
    ##  [4,] 59.33
    ##  [5,] 57.71
    ##  [6,] 62.51
    ##  [7,] 53.75
    ##  [8,] 53.99
    ##  [9,] 55.29
    ## [10,] 58.98
    ## 
    ## [[10]][[1]]$pv
    ##           1
    ##  [1,] 55.18
    ##  [2,] 56.62
    ##  [3,] 54.34
    ##  [4,] 59.33
    ##  [5,] 57.71
    ##  [6,] 62.51
    ##  [7,] 53.75
    ##  [8,] 53.99
    ##  [9,] 55.29
    ## [10,] 58.98
    ## 
    ## 
    ## 
    ## [[11]]
    ## [[11]][[1]]
    ## [[11]][[1]]$ev
    ##           1
    ##  [1,] 62.34
    ##  [2,] 58.39
    ##  [3,] 58.70
    ##  [4,] 60.82
    ##  [5,] 62.41
    ##  [6,] 63.16
    ##  [7,] 62.86
    ##  [8,] 61.26
    ##  [9,] 60.75
    ## [10,] 62.34
    ## 
    ## [[11]][[1]]$pv
    ##           1
    ##  [1,] 62.34
    ##  [2,] 58.39
    ##  [3,] 58.70
    ##  [4,] 60.82
    ##  [5,] 62.41
    ##  [6,] 63.16
    ##  [7,] 62.86
    ##  [8,] 61.26
    ##  [9,] 60.75
    ## [10,] 62.34



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



