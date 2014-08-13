
.. _installation:

Installation & Quickstart
=========================

Zelig 5.0 is the latest version of the Zelig framework for interfacing with a wide range of statistical models and analytic methods in the R statistical programming enviornment. This release expands the set of models available, while simplifying the model wrapping process, and solving architectural problems by completely rewriting into R’s Reference Classes for a fully object-oriented architecture.


This guide is designed to get you up and running with the current *alpha* release, Zelig 5.0.1. For more detailed tutorials see individual model vignettes in the :ref:`userguide`.

------------

Installation
~~~~~~~~~~~~

To use Zelig, you will need to install the R statistical program and the Zelig package:

**Installing R**

To install R, go to http://www.r-project.org/ and select the ``CRAN`` option from the left-hand menu. Select a CRAN mirror closest to your current geographic location and follow the instructions for donwloading R for Linux, (Mac) OS X, or Windows. 

------------

**Installing Zelig**

Becuase Zelig 5.0.1 is still in *alpha* and is not yet available on ``CRAN``, it must be downloaded from Github using the ``devtools`` package. Once R is installed, open the terminal and type in the following commands:

.. sourcecode:: r
  
	> install.packages("devtools") # Install devtools package, if not already installed
	> library(devtools) # Load devtools
	> install_github('IQSS/Zelig5') # Download Zelig 5.0.1 from the IQSS Github repo

Loading Zelig, by typing ``library(Zelig)`` into the R command line, will also load optional libaries and (for the most part) install required dependencies. Therefore, it is not necessary to load any package other than ``Zelig`` at startup.

------------

**Updating Zelig**

To update your Zelig installation to the latest release use the ``updated.packages()`` function.


------------

Quickstart Guide
~~~~~~~~~~~~~~~~

**Loading Zelig**

After installing both R and Zelig, Zelig can be loaded like any other R package:
::
	> library(Zelig5)

Additionally, some Zelig models require add-on packages/modules which can be installed using ``install.packages()``:

.. sourcecode:: r
  
	> install.packages("ZeligChoice") #install ZeligChoice add-on package
	> library(ZeligChoice)

------------

Running Models
~~~~~~~~~~~~~~
Each Zelig process consists of three methods:

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


The same model can also be implemented using the ``zelig()``, ``setx()``, and ``sim()`` functions, as was the case in previous versions of Zelig:

.. sourcecode:: r

	> z.out <- zelig(dist ~ speed, model = "ls", data = cars)
	> x.out <- setx(z.out, speed = 30)
	> s.out <- sim(z.out, x = x.out, num = 1000)

For a complete listing of all supported models, including links to tutorials, see the Zelig model reference.
see the zelig model reference which includes links to model implementation

------------

**Quantities of Interest**

A major feature of Zelig is the translation of model estimates into interpretable quantities of interest (qi's). These qi's, which include expected and predicted values as well as first differences, can be accessed via the ``$sim.out`` method:

.. sourcecode:: r
	
	> z5$sim.out #or
	> summary(s.out)

------------

**Plots**

A second major Zelig feature is that the software makes presenting qi's easy. Using the ``plot()`` function with the Zelig model object (e.g., ``z5`` or ``s.out`` objects above) will produce ready-to-use plots with labels and confidence intervals:

.. Screenshot of plots
















