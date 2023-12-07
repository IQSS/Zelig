Everyone's Statistical Software
==========================================

[![CRAN Version](http://www.r-pkg.org/badges/version/Zelig)](http://cran.r-project.org/package=Zelig)
[![Travis (LINUX) Build Status](https://travis-ci.org/IQSS/Zelig.svg?branch=master)](https://travis-ci.org/IQSS/Zelig)
[![AppVeyor (Windows) Build Status](https://ci.appveyor.com/api/projects/status/github/IQSS/Zelig?branch=master&svg=true)](https://ci.appveyor.com/project/IQSS/Zelig)
[![codecov](https://codecov.io/gh/IQSS/Zelig/branch/master/graph/badge.svg)](https://codecov.io/gh/IQSS/Zelig)


<img style="float:left; padding-right:20px" src="https://raw.githubusercontent.com/IQSS/Zelig/master/man/figures/zelig.png"/>

Zelig is an easy-to-use, free, open source, general purpose statistics program for estimating, interpreting, and presenting results from any statistical method. Zelig turns the power of [R](https://www.r-project.org/), with thousands of open source packages — but with free ranging syntax, diverse examples, and documentation written for different audiences — into the same three functions and consistent documentation for every method. Zelig uses R code from many researchers, making it "everyone's statistical software". We hope it becomes everyone's statistical software for applications too, as we designed it so anyone can use it or add their methods to it. We aim for Zelig to be the best way to do analysis, prepare replication files, learn new methods, or teach.

## Important Update!

*Zelig is no longer being maintained, but a new package, [`clarify`](https://cran.r-project.org/package=clarify), is available to provide much of Zelig's functionality for simulation-based inference of interpretable post-estimation quantities. Please see the `clarify` [website](https://iqss.github.io/clarify/) for information on installing and using `clarify` and for performing tasks in Zelig workflow using `clarify`.*

## Details

Zelig includes many methods, based on likelihood, frequentist, Bayesian, robust Bayesian, nonparametric, and population and superpopulation theories of inference. 

<figure>
  <a href="articles/available_models_overview.html"><img src="https://github.com/IQSS/Zelig/blob/pkgdown/README_files/img/zelig_models_thumb.png?raw=true">
  <figcaption>Explore models supported in Zelig 5</figcaption>
  </a>

</figure>

Zelig adds considerable infrastructure to improve the use of existing methods. It translates hard-to-interpret coefficients into meaningful quantities of interest, along with the uncertainty estimates (generalizing [Clarify](http://gking.harvard.edu/publications/clarify-software-interpreting-and-presenting-statistical-results) for Stata); automates graphics and numerical summaries for all models. 


Zelig can also evaluate counterfactuals (via [WhatIf](http://gking.harvard.edu/whatif)); combine multiply imputed data sets to impute missing data and correct for measurement error (via [Amelia](http://gking.harvard.edu/amelia) and [other multiple imputation functions](reference/to_zelig_mi.html)); automates bootstrapping for all models; allows for matching for causal inference to reduce model dependence (via [MatchIt](http://gking.harvard.edu/publications/matchit-nonparametric-preprocessing-parametric-causal-inference) and [cem](http://gking.harvard.edu/cem));  and generates replication data files (for [Dataverse](http://dataverse.org/) to satisfy community replication standards). 

## Find out what's new

To find out what's new in the most recent version of Zelig, checkout the
[NEWS](https://github.com/IQSS/Zelig/blob/master/NEWS.md).

## Get involved!

We love to get feedback on how to improve Zelig.

<i class="fa fa-comment-o" aria-hidden="true"></i> If you have questions and 
suggestions improvements, we'd love to hear from you! You can get in touch via: 
<a href="https://github.com/IQSS/Zelig/issues">GitHub <i class="fa fa-github" aria-hidden="true"></i></a>. 

<i class="fa fa-code" aria-hidden="true"></i> You can even make your R packages 
usable from Zelig by writinga few simple bridge functions. Checkout the 
[Developer's Guide](articles/developers_guide.html) for details.
