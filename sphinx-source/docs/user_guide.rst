.. _userguide:

User Guide
----------

Zelig 5.0 is the latest version of the Zelig framework for interfacing with a wide range of statistical models and analytic methods in the R statistical programming enviornment. This release expands the set of models available, while simplifying the model wrapping process, and solving architectural problems by completely rewriting into R’s Reference Classes for a fully object-oriented architecture.


.. _devguide_writing_new_models:
.. include:: installation.rst

------------

Zelig Model Reference
======================

The following models hare currenlty supported in Zelig 5.0:

- Exponential Regression: ``zexp$new()``
- Gamma Regression: ``zgamma()``
- Logistic Regression: ``zlogit$new()``
- Log-Normal Regression: ``zlognorm$new()``
- Least Squares Regression: ``zls$new()``
- Negative Binomial Regression: ``zbinom$new()``
- Normal Regression: ``znormal$new()``
- Poisson Regression: ``zpoisson$new()``
- Probit Regression: ``zprobit$new()``
- Quantile Regression: ``zquantile$new()``
- Rare Events Logistic Regression: ``zrelogit$new()``
- Tobit Regression: ``ztobit$new()``


------------

Zelig Model Vignettes
=====================

------------

Exponential Regression for Duration Dependent Variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Use the exponential duration regression model if you have a dependent variable representing a duration (time until an event). The model assumes a constant hazard rate for all events. The dependent variable may be censored (for observations have not yet been completed when data were collected).

**Syntax**

.. sourcecode:: r

    > z.out <- zelig(Surv(Y, C) ~ X, model = "exp", data = mydata)
    > x.out <- setx(z.out)
    > s.out <- sim(z.out, x = x.out)

Exponential models require that the dependent variable be in the form ``Surv(Y, C)``, where ``Y`` and ``C`` are vectors of length ``n``. For each observation ``i`` in 1, …, ``n``, the value $y_i$ is the duration (lifetime, for example), and the associated $c_i$ is a binary variable such that $c_i = 1$ if the duration is not censored (`*e.g.*`, the subject dies during the study) or $c_i = 0$ if the duration is censored (`*e.g.*`, the subject is still alive at the end of the study and is know to live at least as long as $y_i$). If $c_i$ is omitted, all Y are assumed to be completed; that is, time defaults to 1 for all observations.

**Input Values**

In addition to the standard inputs, `zelig()` takes the following additional options for exponential regression:

-   `robust`: defaults to `FALSE`. If `TRUE`, `zelig()` computes robust standard errors based on sandwich estimators (see @Huber81 and @White80) and the options selected in `cluster`.