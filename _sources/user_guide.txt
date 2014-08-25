.. _userguide:

User Guide
----------

Zelig 5.0 is the latest version of the Zelig framework for interfacing with a wide range of statistical models and analytic methods in the R statistical programming enviornment. This release expands the set of models available, while simplifying the model wrapping process, and solving architectural problems by completely rewriting into R’s Reference Classes for a fully object-oriented architecture.


.. _devguide_writing_new_models:
.. include:: installation.rst

------------

Zelig Model Reference
======================

The following models hare currently supported in Zelig 5.0:

- `Exponential Regression <http://zeligproject.org/vignette.html#zelig-exp>`_: ``zexp$new()``
- `Gamma Regression <http://zeligproject.org/vignette.html#zelig-gamma>`_: ``zgamma()``
- `Logistic Regression <http://zeligproject.org/vignette.html#zelig-logit>`_: ``zlogit$new()``
- `Log Normal Regression <http://zeligproject.org/vignette.html#zelig-lognorm>`_: ``zlognorm$new()``
- `Least Squares Regression <http://zeligproject.org/vignette.html#zelig-ls>`_: ``zls$new()``
- `Negative Binomial Regression <http://zeligproject.org/vignette.html#zelig-exp>`_: ``zbinom$new()``
- `Normal Regression <http://zeligproject.org/vignette.html#zelig-normal>`_: ``znormal$new()``
- `Poisson Regression <http://zeligproject.org/vignette.html#zelig-poisson>`_: ``zpoisson$new()``
- `Probit Regression <http://zeligproject.org/vignette.html#zelig-probit>`_: ``zprobit$new()``
- `Quantile Regression <http://zeligproject.org/vignette.html#zelig-quantile>`_: ``zquantile$new()``
- `Rare Events Logistic Regression <http://zeligproject.org/vignette.html#zelig-relogit>`_: ``zrelogit$new()``
- `Tobit Regression <http://zeligproject.org/vignette.html#zelig-tobit>`_: ``ztobit$new()``


------------

Zelig Model Vignettes
=====================

.. include:: vignettes/Zelig-ls.rst
