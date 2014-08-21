normal: Normal Regression for Continuous Dependent Variables
============================================================

The Normal regression model is a close variant of the more standard
least squares regression model (see ). Both models specify a continuous
dependent variable as a linear function of a set of explanatory
variables. The Normal model reports maximum likelihood (rather than
least squares) estimates. The two models differ only in their estimate
for the stochastic parameter :math:`\sigma`.

Syntax
~~~~~~

::

    > z.out <- zelig(Y ~ X1 + X2, model = "normal", data = mydata)
    > x.out <- setx(z.out)
    > s.out <- sim(z.out, x = x.out)

Additional Inputs
~~~~~~~~~~~~~~~~~

In addition to the standard inputs, zelig() takes the following
additional options for normal regression:

-  robust: defaults to FALSE. If TRUE is selected, zelig() computes
   robust standard errors via the sandwich package (see ). The default
   type of robust standard error is heteroskedastic and autocorrelation
   consistent (HAC), and assumes that observations are ordered by time
   index.

   In addition, robust may be a list with the following options:

   -  method: Choose from

      -  “vcovHAC”: (default if robust = TRUE) HAC standard errors.

      -  “kernHAC”: HAC standard errors using the weights given in .

      -  “weave”: HAC standard errors using the weights given in .

   -  order.by: defaults to NULL (the observations are chronologically
      ordered as in the original data). Optionally, you may specify a
      vector of weights (either as order.by = z, where z exists outside
      the data frame; or as order.by = ~z, where z is a variable in the
      data frame). The observations are chronologically ordered by the
      size of z.

   -  …: additional options passed to the functions specified in method.
      See the sandwich library and for more options.

Examples
~~~~~~~~

#. Basic Example with First Differences

   Attach sample data:

   RRR> data(macro)

   Estimate model:

   RRR> z.out1 <- zelig(unem   gdp + capmob + trade, model = “normal”, +
   data = macro)

   Summarize of regression coefficients:

   RRR> summary(z.out1)

   Set explanatory variables to their default (mean/mode) values, with
   high (80th percentile) and low (20th percentile) values for trade:

   RRR> x.high <- setx(z.out1, trade =
   quantile(macro\ :math:`trade, 0.8))
   RRR>  x.low <- setx(z.out1, trade = quantile(macro`\ trade, 0.2))

   Generate first differences for the effect of high versus low trade on
   GDP:

   RRR> s.out1 <- sim(z.out1, x = x.high, x1 = x.low)

   RRR> summary(s.out1)

   A visual summary of quantities of interest:

   RRR> plot(s.out1)

   |image|

Model
~~~~~

Let :math:`Y_i` be the continuous dependent variable for observation
:math:`i`.

-  The *stochastic component* is described by a univariate normal model
   with a vector of means :math:`\mu_i` and scalar variance
   :math:`\sigma^2`:

   .. math:: Y_i \; \sim \; \textrm{Normal}(\mu_i, \sigma^2).

-  The *systematic component* is

   .. math:: \mu_i \;= \; x_i \beta,

   where :math:`x_i` is the vector of :math:`k` explanatory variables
   and :math:`\beta` is the vector of coefficients.

Quantities of Interest
~~~~~~~~~~~~~~~~~~~~~~

-  The expected value (qi$ev) is the mean of simulations from the the
   stochastic component,

   .. math:: E(Y) = \mu_i = x_i \beta,

   given a draw of :math:`\beta` from its posterior.

-  The predicted value (qi$pr) is drawn from the distribution defined by
   the set of parameters :math:`(\mu_i, \sigma)`.

-  The first difference (qi$fd) is:

   .. math:: \textrm{FD}\; = \;E(Y \mid x_1) -  E(Y \mid x)

-  In conditional prediction models, the average expected treatment
   effect (att.ev) for the treatment group is

   .. math::

      \frac{1}{\sum_{i=1}^n t_i}\sum_{i:t_i=1}^n \left\{ Y_i(t_i=1) -
            E[Y_i(t_i=0)] \right\},

   where :math:`t_i` is a binary explanatory variable defining the
   treatment (:math:`t_i=1`) and control (:math:`t_i=0`) groups.
   Variation in the simulations are due to uncertainty in simulating
   :math:`E[Y_i(t_i=0)]`, the counterfactual expected value of
   :math:`Y_i` for observations in the treatment group, under the
   assumption that everything stays the same except that the treatment
   indicator is switched to :math:`t_i=0`.

-  In conditional prediction models, the average predicted treatment
   effect (att.pr) for the treatment group is

   .. math::

      \frac{1}{\sum_{i=1}^n t_i}\sum_{i:t_i=1}^n \left\{ Y_i(t_i=1) -
            \widehat{Y_i(t_i=0)} \right\},

   where :math:`t_i` is a binary explanatory variable defining the
   treatment (:math:`t_i=1`) and control (:math:`t_i=0`) groups.
   Variation in the simulations are due to uncertainty in simulating
   :math:`\widehat{Y_i(t_i=0)}`, the counterfactual predicted value of
   :math:`Y_i` for observations in the treatment group, under the
   assumption that everything stays the same except that the treatment
   indicator is switched to :math:`t_i=0`.

Output Values
~~~~~~~~~~~~~

The output of each Zelig command contains useful information which you
may view. For example, if you run
``z.out <- zelig(y ~ x, model = normal, data)``, then you may examine
the available information in ``z.out`` by using ``names(z.out)``, see
the coefficients by using z.out$coefficients, and a default summary of
information through ``summary(z.out)``. Other elements available through
the $ operator are listed below.

-  From the zelig() output object z.out, you may extract:

   -  coefficients: parameter estimates for the explanatory variables.

   -  residuals: the working residuals in the final iteration of the
      IWLS fit.

   -  fitted.values: fitted values. For the normal model, these are
      identical to the linear predictors.

   -  linear.predictors: fitted values. For the normal model, these are
      identical to fitted.values.

   -  aic: Akaike’s Information Criterion (minus twice the maximized
      log-likelihood plus twice the number of coefficients).

   -  df.residual: the residual degrees of freedom.

   -  df.null: the residual degrees of freedom for the null model.

   -  zelig.data: the input data frame if save.data = TRUE.

-  From summary(z.out), you may extract:

   -  coefficients: the parameter estimates with their associated
      standard errors, :math:`p`-values, and :math:`t`-statistics.

   -  cov.scaled: a :math:`k \times k` matrix of scaled covariances.

   -  cov.unscaled: a :math:`k \times k` matrix of unscaled covariances.

-  From the sim() output object s.out, you may extract quantities of
   interest arranged as matrices indexed by simulation :math:`\times`
   x-observation (for more than one x-observation). Available quantities
   are:

   -  qi$ev: the simulated expected values for the specified values of
      x.

   -  qi$pr: the simulated predicted values drawn from the distribution
      defined by :math:`(\mu_i, \sigma)`.

   -  qi$fd: the simulated first difference in the simulated expected
      values for the values specified in x and x1.

   -  qi$att.ev: the simulated average expected treatment effect for the
      treated from conditional prediction models.

   -  qi$att.pr: the simulated average predicted treatment effect for
      the treated from conditional prediction models.

How to Cite
-----------

See also
--------

The normal model is part of the stats package by . Advanced users may
wish to refer to ``help(glm)`` and ``help(family)``, as well as . Robust
standard errors are implemented via the sandwich package by . Sample
data are from .

.. |image| image:: vigpics/normal-ExamplesPlot
