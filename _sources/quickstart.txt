.. _quickstart:

Quickstart Guide
----------------

Zelig 5.0 is the latest version of the Zelig framework for interfacing with a wide range of statistical models and analytic methods in the R statistical programming enviornment. This release expands the set of models available, while simplifying the model wrapping process, and solving architectural problems by completely rewriting into R’s Reference Classes for a fully object-oriented architecture.


This quickstart guide is designed to get you up and running with Zelig 5.0. For more detailed tutorials see individual model vignettes in the :ref:`userguide`.

------------

Loading Zelig
~~~~~~~~~~~~~
After installing R and Zelig (see :ref:`installation` page). Once installed, Zelig can be loaded like any other R package:
::
	> library(Zelig5)

Additionally, some Zelig models require add-on packages which can be installed using ``install.packages()``:

.. sourcecode:: r
  
	> install.packages("ZeligChoice") #install ZeligChoice add-on package
	> libarry(ZeligChoice)

------------

Running Models
~~~~~~~~~~~~~~
Each Zelig process consists of three component methods:

1. Specify statistical model and estimate parameters: ``$zelig``
2. Set explanatory variables to chosen (actual or counterfactual) values for calculating quantities of interest: ``$setx``
3. Draw simulations of quantity of interest from statistical model: ``$sim`` 

For example, to implement a least squares regression:

.. sourcecode:: r

	> data(cars)                            #load toy dataset
	> z5 <- zls$new()                       #initialize Zelig5 least squares object
	> z5$zelig(dist ~ speed, data = cars)   #estimate ls model
	> z5$setx(speed = 30)                   #set speed to 30 (all other covariates set to means)
	> z5$sim(num = 1000)                    #run 1000 simulations and estiamte quantities of interest


The same model can also be implemented using the ``zelig()``, ``setx()``, and ``sim()`` functions:

.. sourcecode:: r

	> z.out <- zelig(dist ~ speed, model = "ls", data = cars)
	> x.out <- setx(z.out, speed = 30)
	> s.out <- sim(z.out, x = x.out, num = 1000)

------------

.. _qis:

**Quantities of Interest**

.. sourcecode:: r

	> summary(sim.out) #or
	> summary(s.out)

------------

.. _plots:

**Plots**

*Coming Soon!*

------------

.. _modelreference:

Zelig5 Model Reference
~~~~~~~~~~~~~~~~~~~~~~
At present, the following models have been tested and implemented in Zelig5:

- Least Squares Regression: ``zls$new()`` or ``model = "ls"``
- Logistic Regression: ``zlogit$new()`` or ``model = "logit"``

The following models have been implemented **but have not been unit-tested**:

- Tobit Regression: