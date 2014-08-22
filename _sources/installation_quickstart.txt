
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
    ##  [1,] 20.11
    ##  [2,] 25.34
    ##  [3,] 25.71
    ##  [4,] 20.79
    ##  [5,] 21.98
    ##  [6,] 25.66
    ##  [7,] 24.53
    ##  [8,] 15.14
    ##  [9,] 22.30
    ## [10,] 17.73
    ## 
    ## [[1]][[1]]$pv
    ##           1
    ##  [1,] 20.11
    ##  [2,] 25.34
    ##  [3,] 25.71
    ##  [4,] 20.79
    ##  [5,] 21.98
    ##  [6,] 25.66
    ##  [7,] 24.53
    ##  [8,] 15.14
    ##  [9,] 22.30
    ## [10,] 17.73
    ## 
    ## 
    ## 
    ## [[2]]
    ## [[2]][[1]]
    ## [[2]][[1]]$ev
    ##           1
    ##  [1,] 28.41
    ##  [2,] 24.76
    ##  [3,] 29.06
    ##  [4,] 23.55
    ##  [5,] 19.08
    ##  [6,] 21.43
    ##  [7,] 24.13
    ##  [8,] 30.23
    ##  [9,] 28.34
    ## [10,] 27.65
    ## 
    ## [[2]][[1]]$pv
    ##           1
    ##  [1,] 28.41
    ##  [2,] 24.76
    ##  [3,] 29.06
    ##  [4,] 23.55
    ##  [5,] 19.08
    ##  [6,] 21.43
    ##  [7,] 24.13
    ##  [8,] 30.23
    ##  [9,] 28.34
    ## [10,] 27.65
    ## 
    ## 
    ## 
    ## [[3]]
    ## [[3]][[1]]
    ## [[3]][[1]]$ev
    ##           1
    ##  [1,] 26.09
    ##  [2,] 28.63
    ##  [3,] 26.89
    ##  [4,] 37.01
    ##  [5,] 25.64
    ##  [6,] 25.64
    ##  [7,] 31.36
    ##  [8,] 26.56
    ##  [9,] 29.76
    ## [10,] 25.38
    ## 
    ## [[3]][[1]]$pv
    ##           1
    ##  [1,] 26.09
    ##  [2,] 28.63
    ##  [3,] 26.89
    ##  [4,] 37.01
    ##  [5,] 25.64
    ##  [6,] 25.64
    ##  [7,] 31.36
    ##  [8,] 26.56
    ##  [9,] 29.76
    ## [10,] 25.38
    ## 
    ## 
    ## 
    ## [[4]]
    ## [[4]][[1]]
    ## [[4]][[1]]$ev
    ##           1
    ##  [1,] 32.47
    ##  [2,] 33.92
    ##  [3,] 30.75
    ##  [4,] 32.10
    ##  [5,] 33.76
    ##  [6,] 31.94
    ##  [7,] 33.85
    ##  [8,] 33.76
    ##  [9,] 34.30
    ## [10,] 30.65
    ## 
    ## [[4]][[1]]$pv
    ##           1
    ##  [1,] 32.47
    ##  [2,] 33.92
    ##  [3,] 30.75
    ##  [4,] 32.10
    ##  [5,] 33.76
    ##  [6,] 31.94
    ##  [7,] 33.85
    ##  [8,] 33.76
    ##  [9,] 34.30
    ## [10,] 30.65
    ## 
    ## 
    ## 
    ## [[5]]
    ## [[5]][[1]]
    ## [[5]][[1]]$ev
    ##           1
    ##  [1,] 36.24
    ##  [2,] 35.51
    ##  [3,] 34.35
    ##  [4,] 36.12
    ##  [5,] 35.25
    ##  [6,] 37.76
    ##  [7,] 39.89
    ##  [8,] 37.85
    ##  [9,] 32.95
    ## [10,] 40.37
    ## 
    ## [[5]][[1]]$pv
    ##           1
    ##  [1,] 36.24
    ##  [2,] 35.51
    ##  [3,] 34.35
    ##  [4,] 36.12
    ##  [5,] 35.25
    ##  [6,] 37.76
    ##  [7,] 39.89
    ##  [8,] 37.85
    ##  [9,] 32.95
    ## [10,] 40.37
    ## 
    ## 
    ## 
    ## [[6]]
    ## [[6]][[1]]
    ## [[6]][[1]]$ev
    ##           1
    ##  [1,] 37.27
    ##  [2,] 40.06
    ##  [3,] 40.62
    ##  [4,] 42.34
    ##  [5,] 44.23
    ##  [6,] 40.85
    ##  [7,] 44.70
    ##  [8,] 45.08
    ##  [9,] 42.18
    ## [10,] 41.68
    ## 
    ## [[6]][[1]]$pv
    ##           1
    ##  [1,] 37.27
    ##  [2,] 40.06
    ##  [3,] 40.62
    ##  [4,] 42.34
    ##  [5,] 44.23
    ##  [6,] 40.85
    ##  [7,] 44.70
    ##  [8,] 45.08
    ##  [9,] 42.18
    ## [10,] 41.68
    ## 
    ## 
    ## 
    ## [[7]]
    ## [[7]][[1]]
    ## [[7]][[1]]$ev
    ##           1
    ##  [1,] 47.04
    ##  [2,] 42.78
    ##  [3,] 43.92
    ##  [4,] 43.57
    ##  [5,] 43.04
    ##  [6,] 43.74
    ##  [7,] 48.83
    ##  [8,] 39.36
    ##  [9,] 45.76
    ## [10,] 44.87
    ## 
    ## [[7]][[1]]$pv
    ##           1
    ##  [1,] 47.04
    ##  [2,] 42.78
    ##  [3,] 43.92
    ##  [4,] 43.57
    ##  [5,] 43.04
    ##  [6,] 43.74
    ##  [7,] 48.83
    ##  [8,] 39.36
    ##  [9,] 45.76
    ## [10,] 44.87
    ## 
    ## 
    ## 
    ## [[8]]
    ## [[8]][[1]]
    ## [[8]][[1]]$ev
    ##           1
    ##  [1,] 47.81
    ##  [2,] 49.13
    ##  [3,] 46.38
    ##  [4,] 52.59
    ##  [5,] 48.84
    ##  [6,] 48.80
    ##  [7,] 48.94
    ##  [8,] 48.92
    ##  [9,] 49.90
    ## [10,] 49.89
    ## 
    ## [[8]][[1]]$pv
    ##           1
    ##  [1,] 47.81
    ##  [2,] 49.13
    ##  [3,] 46.38
    ##  [4,] 52.59
    ##  [5,] 48.84
    ##  [6,] 48.80
    ##  [7,] 48.94
    ##  [8,] 48.92
    ##  [9,] 49.90
    ## [10,] 49.89
    ## 
    ## 
    ## 
    ## [[9]]
    ## [[9]][[1]]
    ## [[9]][[1]]$ev
    ##           1
    ##  [1,] 50.52
    ##  [2,] 55.26
    ##  [3,] 53.92
    ##  [4,] 54.09
    ##  [5,] 54.11
    ##  [6,] 47.59
    ##  [7,] 54.12
    ##  [8,] 51.60
    ##  [9,] 55.30
    ## [10,] 45.68
    ## 
    ## [[9]][[1]]$pv
    ##           1
    ##  [1,] 50.52
    ##  [2,] 55.26
    ##  [3,] 53.92
    ##  [4,] 54.09
    ##  [5,] 54.11
    ##  [6,] 47.59
    ##  [7,] 54.12
    ##  [8,] 51.60
    ##  [9,] 55.30
    ## [10,] 45.68
    ## 
    ## 
    ## 
    ## [[10]]
    ## [[10]][[1]]
    ## [[10]][[1]]$ev
    ##           1
    ##  [1,] 51.66
    ##  [2,] 57.62
    ##  [3,] 56.06
    ##  [4,] 54.90
    ##  [5,] 59.35
    ##  [6,] 59.19
    ##  [7,] 54.81
    ##  [8,] 57.81
    ##  [9,] 58.73
    ## [10,] 59.63
    ## 
    ## [[10]][[1]]$pv
    ##           1
    ##  [1,] 51.66
    ##  [2,] 57.62
    ##  [3,] 56.06
    ##  [4,] 54.90
    ##  [5,] 59.35
    ##  [6,] 59.19
    ##  [7,] 54.81
    ##  [8,] 57.81
    ##  [9,] 58.73
    ## [10,] 59.63
    ## 
    ## 
    ## 
    ## [[11]]
    ## [[11]][[1]]
    ## [[11]][[1]]$ev
    ##           1
    ##  [1,] 64.01
    ##  [2,] 62.98
    ##  [3,] 59.22
    ##  [4,] 63.05
    ##  [5,] 59.92
    ##  [6,] 56.10
    ##  [7,] 59.65
    ##  [8,] 64.46
    ##  [9,] 60.59
    ## [10,] 64.22
    ## 
    ## [[11]][[1]]$pv
    ##           1
    ##  [1,] 64.01
    ##  [2,] 62.98
    ##  [3,] 59.22
    ##  [4,] 63.05
    ##  [5,] 59.92
    ##  [6,] 56.10
    ##  [7,] 59.65
    ##  [8,] 64.46
    ##  [9,] 60.59
    ## [10,] 64.22



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



