
.. _installation:

Installation
============

To use Zelig, you are going to need to install the R statistical program, the Zelig package, and some R libraries (e.g., coda, MCMCpack, sandwich, VGAM, and zoo):

------------

Installing R
------------
To install R, go to the `r-project website <www.r-project.org>`_ and select the ``CRAN`` option on the left-hand menu. Select a CRAN mirror mirror closest to your current location and donwload R for Linux, (Mac) OS X, or Windows. 

------------

Installing Zelig
----------------
Once R is installed, you must install the Zelig and VGAM packages. There are three ways to do this.

1. We recommend that you start R and install Zelig and related dependencies by typing:
::
	> source("http://gking.harvard.edu/zelig/install.R")
	> library(Zelig)

2. Alternatively, you may install each component package individually:
::
	> install.packages("Zelig")
	> install.packages("zoo")
	> install.packages("sandwich")
	> install.packages("MCMCpack")
	> install.packages("coda")
	> install.packages("lattice")
	> install.packages("mvtnorm")
	> install.packages("VGAM")
	> install.packages("sna")
	> install.packages("systemfit")
	> install.packages("nnet")
	> install.packages("gee")
	> install.packages("mgcv")

Loading Zelig by typing ``library(Zelig)`` into the R command line will also load the optional libaries just installed. Therefore, it is not necessary to load any package other than ``Zelig`` at startup.

------------

Updating Zelig
--------------
There are two ways to update your Zelig installation to the latest release:

1. Via the R command line:
::
	> update.packages()

2. You can also remove an old version by typing ``R CMD REMOVE Zelig`` at the terminal prompt and manually downloading and reinstalling the latest package release following the instructions outlined above.
::