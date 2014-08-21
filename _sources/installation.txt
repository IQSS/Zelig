
.. _installation:

Installation & Quickstart
=========================

This guide is designed to get you up and running with the current *alpha* release of Zelig 5. 

Note: In code snippets,``>`` refers to an R terminal prompt and anything after ``#`` is a comment meant to describe what the code is doing.

------------

Installation
~~~~~~~~~~~~

Before using Zelig, you will need to download and install both the R statistical program and the Zelig package:

**Installing R**

To install R, go to www.r-project.org/  Select the ``CRAN`` option from the left-hand menu (CRAN is the Comprehensive R Archive Network where all files related to R can be found). Select a CRAN mirror closest to your current geographic location (there are multiple mirrors of this database in various locations, selecting the one closest to you will be sure to maximize your downloading speeds).  Follow the instructions for downloading R for Linux, Mac OS X, or Windows. 

------------

**Installing Zelig**

Becuase Zelig 5.0.1 is still an *alpha* release and is not yet available on ``CRAN`` (with other R software packages), it must be downloaded from Github using the ``devtools`` package.

Once you've successfully installed R, open it, and at the terminal prompt, type in the following commands verbatim:

.. sourcecode:: r

  	# This installs devtools package, if not already installed
	> install.packages("devtools")
	# This loads devtools   	
	> library(devtools)
	# This downloads Zelig 5.0.1 from the IQSS Github repo
	> install_github('IQSS/Zelig5')   	

Once you have successfully typed these commands, you will see a the following message: *"DONE (Zelig5)"*.

------------

Quickstart Guide
~~~~~~~~~~~~~~~~

**Loading Zelig**

After installing both R and Zelig, Zelig can be loaded by using the ``library()`` function:

.. sourcecode:: r
	
	> library(Zelig5)

------------

Running Models
~~~~~~~~~~~~~~
Imagine a scenario in which you want to predict how the distance a car can travel given it's speed. If we were to model this relationship using a least squares regression within Zelig we need to follow three steps:

1. First, we are going to want to specify our model, given a dataset on cars including distance and speed, and estimate the effect of speed on distance. This is done using the ``$zelig()`` method in the code snippet below.

2. Second, we want to translate our estimates into intepretable quanities of interest, so we can answer intuitive questions about the effect of speed on distance. For example, we may be interested into understanding how a change of speed from 10 to 20 mph affects distance versus a change from 50 to 60 mph. To do this we have to set explanatory variables in our model (i.e., speed) to simulate quantities of interest. This is done using the ``$setx()`` or ``$setrange()`` method.

3. Finally, we want to draw simualtions of quantieis of interest from our statistical model using the ``$sim()`` method.

The following code snippet loads a data set of cars data including speed and distance (When you install R, example datasets are also installed), and regressing distance on speed. We then go on to set speed (our main explanatory variable) and simulate quantities of interest.

.. sourcecode:: r

	#load toy dataset
	> data(cars)
	#initialize Zelig5 least squares object                            
	> z5 <- zls$new()  
	#estimate ls model                     
	> z5$zelig(dist ~ speed, data = cars)   
	
	#set speed to 30 (all other covariates set to means)
	> z5$setx(speed = 30)   
	#or, simulate over a range of speed between 55 and 80
	> z5$setrange(speed = 55:80)                 
	
	#run 1000 simulations and estiamte quantities of interest
	> z5$sim(num = 10)

	#print model estimates
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
------------

**Quantities of Interest**

A major feature of Zelig is the translation of model estimates into easy to interpret quantities of interest (QIs). These QIs (e.g. expected and predicted values) can be accessed via the ``$sim.out`` method:

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

.. image:: gr1.png

------------

*Plot of expected values across range of simulations:*

.. image:: gr3.png