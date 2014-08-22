
.. _installation_quickstart:

Installation & Quickstart
=========================

This guide is designed to get you up and running with the current *alpha* release of Zelig (5.0.1). 

``Note: In code snippets,">" refers to the R terminal prompt and anything after "#" is a comment`` 

``meant to describe what the code is doing.``

------------

Installing R & Zelig
~~~~~~~~~~~~~~~~~~~~

Before using Zelig, you will need to download and install both the R statistical program and the Zelig package:

**Installing R**

To install R, go to `http://www.r-project.org/ <http://www.r-project.org/>`_  Select the ``CRAN`` option from the left-hand menu (CRAN is the Comprehensive R Archive Network where all files related to R can be found). Pick a CRAN mirror closest to your current geographic location (there are multiple mirrors of this database in various locations, selecting the one closest to you will be sure to maximize your the speed of your download).  Follow the instructions for downloading R for Linux, Mac OS X, or Windows. 

------------

**Installing Zelig**

Because Zelig 5 is still an *alpha* release and is not yet available on ``CRAN`` (with other R software packages), it must be downloaded from Github using the ``devtools`` package.

Once you've successfully installed R, open it, and at the terminal prompt, type in the following commands verbatim:

.. sourcecode:: r

  	# This installs devtools package, if not already installed
	> install.packages("devtools")
	# This loads devtools   	
	> library(devtools)
	# This downloads Zelig 5.0.1 from the IQSS Github repo
	> install_github('IQSS/Zelig')   	

If you have successfully installed the program, you will see a the following message: *"DONE (Zelig5)"*.

------------

Quickstart Guide
~~~~~~~~~~~~~~~~
Now that we have successfully downloaded and installed Zelig from Github, we will load the package and walk through am example. The scenario is a simple one: imagine you want to estimate the distance a car has traveled given it's speed and you have a dataset of cars, distance and speed. Throughout the rest of this guide, we will walk you through building a statistical model from this data using Zelig. 


**Loading Zelig**

First, we have to load Zelig into R. After installing both R and Zelig, open R. type:

.. sourcecode:: r
	
	# remember the `>` is not code
	> library(Zelig5)

------------

**Building Models**

Now, lets build a statistical model that captures the relationship a cars distance and speed, where distance is the outcome (dependent) variable and speed is the only explanatory (independent) variable. Given that distance (the DV) is a continuous variable, we should use a least squares regression.

To do so, we must first create Zelig least squares object, then specify our model, and finally regress distance on speed to estimate the relationship between speed and distance:

.. sourcecode:: r

	# load toy dataset (when you install R, example datasets are also installed)
	> data(cars)
	# initialize Zelig5 least squares object                            
	> z5 <- zls$new()  
	# estimate ls model                     
	> z5$zelig(dist ~ speed, data = cars)
	# you can now get model summary estimates
	> z5

	Call:
	stats::lm(formula = dist ~ speed, data = cars)

	Residuals:
    	Min      1Q  Median      3Q     Max 
	-29.069  -9.525  -2.272   9.215  43.201 

	Coefficients:
    	        Estimate Std. Error t value Pr(>|t|)    
	(Intercept) -17.5791     6.7584  -2.601   0.0123 *  
	speed         3.9324     0.4155   9.464 1.49e-12 ***
	---
	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

	Residual standard error: 15.38 on 48 degrees of freedom
	Multiple R-squared:  0.6511,	Adjusted R-squared:  0.6438 
	F-statistic: 89.57 on 1 and 48 DF,  p-value: 1.49e-12
	>        

So what do our model estimates tell us? First, off we can see that the positive 3.93 estimate for speed suggests a positive relationship between speed and distance. In particular, we would interpret this coefficient as a one unit increase in speed (e.g., mph) leads to a 3 unit increase in distance (e.g., miles). This interpretation is not very intuitive. Perhaps, we want to know how the distance a car can travel changes over a range of speed (e.g., 10 to 20 mph).

Zelig makes this simple, by automating the translation of model estimates in interpretable quantities of interest (more on this below) using Monte Carlo simulations. To get this process started we need to set explanatory variables in our model (i.e., speed) using the ``$setx()`` or ``$setrange()`` method:

.. sourcecode:: r

	# simulate over a range of speed between 10 and 20 mph
	> z5$setrange(speed = 10:20)

	# you can also set covariates at particular value using $setx()
	> z4$setx(speed = 30)  

Now that we've set our variables, all we have to do is run our simulations:

.. sourcecode:: r          
	
	#run 10 simulations and estimate quantities of interest
	> z5$sim(num = 10)

	#default is 1,000 simulations

Now we've estimated a model and calculated interpretable estimates across a range of speed (e.g., 10 - 20 mph). What can we do with them? Zelig gives you access to estimated quantities of interest and makes plotting and presenting them particularly easy.

------------

**Quantities of Interest**

As mentioned earlier, a major feature of Zelig is the translation of model estimates into easy to interpret quantities of interest (QIs). These QIs (e.g. expected and predicted values) can be accessed via the ``$sim.out`` method:

.. sourcecode:: r
	
	> z5$sim.out
	$range
	$range[[1]]
	$range[[1]][[1]]
	$range[[1]][[1]]$ev
	             1
	 [1,] 201.2214
	 [2,] 186.4815
	 [3,] 187.1505
	 [4,] 175.2489
	 [5,] 187.9555
	 [6,] 197.8107
	 [7,] 199.0940
	 [8,] 168.2691
	 [9,] 195.7094
	[10,] 222.0503

	$range[[1]][[1]]$pv
	             1
	 [1,] 201.2214
	 [2,] 186.4815
	 [3,] 187.1505
	 [4,] 175.2489
	 [5,] 187.9555
	 [6,] 197.8107
	 [7,] 199.0940
	 [8,] 168.2691
	 [9,] 195.7094
	[10,] 222.0503     
	>  
------------

**Plots**

A second major Zelig feature is how easy it is to plot QIs for presentation in slides or an artcle. Using the ``plot()`` function on the ``z5$s.out`` will produce ready-to-use plots with labels and confidence intervals.

*Plots of QI's from binary choice model:*  

.. image:: images/gr1.png

------------

*Plot of expected values across range of simulations:*

.. image:: images/gr3.png



