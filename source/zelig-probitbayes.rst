.. _zprobitbayes:

zelig-probitbayes
~~~~~~

Bayesian Probit Regression

Use the probit regression model for model binary dependent variables
specified as a function of a set of explanatory variables. The model is
estimated using a Gibbs sampler. For other models suitable for binary
response variables, see Bayesian logistic regression, maximum
likelihood logit regression, and maximum likelihood probit regression.

Syntax
+++++

With reference classes:


.. sourcecode:: r
    

    z5 <- zprobitbayes$new()
    z5$zelig(Y ~ X1 + X2, data = mydata)
    z5$setx()
    z5$sim()


With the Zelig 4 compatibility wrappers:


.. sourcecode:: r
    

    z.out <- zelig(Y ~ X1 + X2, model = "probit.bayes", data = mydata)
    x.out <- setx(z.out)
    s.out <- sim(z.out, x = x.out)


Additional Inputs
+++++

Using the following arguments to monitor the Markov chains:

-  ``burnin``: number of the initial MCMC iterations to be discarded
   (defaults to 1,000).

-  ``mcmc``: number of the MCMC iterations after burnin (defaults to
   10,000).

-  ``thin``: thinning interval for the Markov chain. Only every
   ``thin``-th draw from the Markov chain is kept. The value of ``mcmc``
   must be divisible by this value. The default value is 1.

-  ``verbose``: defaults to FALSE. If ``TRUE``, the progress of the
   sampler (every :math:`10\%`) is printed to the screen.

-  ``seed``: seed for the random number generator. The default is ``NA``
   which corresponds to a random seed of 12345.

-  ``beta.start``: starting values for the Markov chain, either a scalar
   or vector with length equal to the number of estimated coefficients.
   The default is ``NA``, such that the maximum likelihood estimates are
   used as the starting values.

Use the following parameters to specify the model’s priors:

-  ``b0``: prior mean for the coefficients, either a numeric vector or a
   scalar. If a scalar value, that value will be the prior mean for all
   the coefficients. The default is 0.

-  ``B0``: prior precision parameter for the coefficients, either a
   square matrix (with the dimensions equal to the number of the
   coefficients) or a scalar. If a scalar value, that value times an
   identity matrix will be the prior precision parameter. The default is
   0, which leads to an improper prior.

Use the following arguments to specify optional output for the model:

-  ``bayes.resid``: defaults to FALSE. If TRUE, the latent Bayesian
   residuals for all observations are returned. Alternatively, users can
   specify a vector of observations for which the latent residuals
   should be returned.

Zelig users may wish to refer to ``help(MCMCprobit)`` for more
information.

Examples
+++++



Basic Example
!!!!!

Attaching the sample dataset:


.. sourcecode:: r
    

    data(turnout)


Estimating the probit regression using ``probit.bayes``:


.. sourcecode:: r
    

    z.out <- zelig(vote ~ race + educate, model = "probit.bayes", 
                   data = turnout, verbose = FALSE)


::

    ## How to cite this model in Zelig:
    ##   Ben Goodrich, Ying Lu. 2013.
    ##   probitbayes: Bayesian Probit Regression for Dichotomous Dependent Variables
    ##   in Kosuke Imai, Gary King, and Olivia Lau, "Zelig: Everyone's Statistical Software,"
    ##   http://zeligproject.org/



Checking for convergence before summarizing the estimates:


.. sourcecode:: r
    

    geweke.diag(z.out coefficients)



.. sourcecode:: r
    

    heidel.diag(z.out coefficients)



.. sourcecode:: r
    

    raftery.diag(z.out coefficients)



.. sourcecode:: r
    

    summary(z.out)


Setting values for the explanatory variables to their sample averages:


.. sourcecode:: r
    

    x.out <- setx(z.out)



Simulating quantities of interest from the posterior distribution given: ``x.out``


.. sourcecode:: r
    

    s.out1 <- sim(z.out, x = x.out)



.. sourcecode:: r
    

    summary(s.out1)


Simulating First Differences
!!!!!

Estimating the first difference (and risk ratio) in individual’s
probability of voting when education is set to be low (25th
percentile) versus high (75th percentile) while all the other variables are held at their default values:


.. sourcecode:: r
    

    x.high <- setx(z.out, educate = quantile(turnout$educate, prob = 0.75))
    x.low <- setx(z.out, educate = quantile(turnout$educate, prob = 0.25))



.. sourcecode:: r
    

    s.out2 <- sim(z.out, x = x.high, x1 = x.low)



.. sourcecode:: r
    

    summary(s.out2)


Model
+++++

Let :math:`Y_{i}` be the binary dependent variable for observation
:math:`i` which takes the value of either 0 or 1.

-  The *stochastic component* is given by

   .. math::

      \begin{aligned}
      Y_{i}  &  \sim & \textrm{Bernoulli}(\pi_{i})\\
      &  = & \pi_{i}^{Y_{i}}(1-\pi_{i})^{1-Y_{i}},\end{aligned}

   where :math:`\pi_{i}=\Pr(Y_{i}=1)`.

-  The *systematic component* is given by

   .. math::

      \begin{aligned}
      \pi_{i}= \Phi(x_i \beta),\end{aligned}

   where :math:`\Phi(\cdot)` is the cumulative density function of the
   standard Normal distribution with mean 0 and variance 1,
   :math:`x_{i}` is the vector of :math:`k` explanatory variables for
   observation :math:`i`, and :math:`\beta` is the vector of
   coefficients.

-  The *prior* for :math:`\beta` is given by

   .. math::

      \begin{aligned}
      \beta \sim \textrm{Normal}_k \left(  b_{0}, B_{0}^{-1} \right)\end{aligned}

   where :math:`b_{0}` is the vector of means for the :math:`k`
   explanatory variables and :math:`B_{0}` is the :math:`k \times k`
   precision matrix (the inverse of a variance-covariance matrix).

Quantities of Interest
+++++

-  The expected values (``qi$ev``) for the probit model are the
   predicted probability of a success:

   .. math::

      \begin{aligned}
      E(Y \mid X) = \pi_{i}= \Phi(x_i \beta),\end{aligned}

   given the posterior draws of :math:`\beta` from the MCMC iterations.

-  The predicted values (``qi$pr``) are draws from the Bernoulli
   distribution with mean equal to the simulated expected value
   :math:`\pi_{i}`.

-  The first difference (``qi$fd``) for the probit model is defined as

   .. math::

      \begin{aligned}
      \text{FD}=\Pr(Y=1\mid X_{1})-\Pr(Y=1\mid X).\end{aligned}

-  The risk ratio (``qi$rr``)is defined as

   .. math::

      \begin{aligned}
      \text{RR}=\Pr(Y=1\mid X_{1})\ /\ \Pr(Y=1\mid X).\end{aligned}

-  In conditional prediction models, the average expected treatment
   effect (``qi$att.ev``) for the treatment group is

   .. math::

      \begin{aligned}
      \frac{1}{\sum t_{i}}\sum_{i:t_{i}=1}[Y_{i}(t_{i}=1)-E[Y_{i}(t_{i}=0)]],\end{aligned}

   where :math:`t_{i}` is a binary explanatory variable defining the
   treatment (:math:`t_{i}=1`) and control (:math:`t_{i}=0`) groups.

-  In conditional prediction models, the average predicted treatment
   effect (``qi$att.pr``) for the treatment group is

   .. math::

      \begin{aligned}
      \frac{1}{\sum t_{i}}\sum_{i:t_{i}=1}[Y_{i}(t_{i}=1)-\widehat{Y_{i}(t_{i}=0)}],\end{aligned}

   where :math:`t_{i}` is a binary explanatory variable defining the
   treatment (:math:`t_{i}=1`) and control (:math:`t_{i}=0`) groups.

Output Values
+++++

The output of each Zelig command contains useful information which you
may view. For example, if you run:


.. sourcecode:: r
    

    z.out <- zelig(y ~ x, model = "probit.bayes", data)


then you may examine the available information in ``z.out`` by using
``names(z.out)``, see the draws from the posterior distribution of the
``coefficients`` by using ``z.out$coefficients``, and view a default
summary of information through ``summary(z.out)``. Other elements
available through the ``$`` operator are listed below.

-  From the ``zelig()`` output object ``z.out``, you may extract:

   -  ``coefficients``: draws from the posterior distributions of the
      estimated parameters.

   -  zelig.data: the input data frame if save.data = TRUE.

   -  ``bayes.residuals``: When ``bayes.residual`` is ``TRUE`` or a set
      of observation numbers is given, this object contains the
      posterior draws of the latent Bayesian residuals of all the
      observations or the observations specified by the user.

   -  ``seed``: the random seed used in the model.

-  From the ``sim()`` output object ``s.out``:

   -  ``qi$ev``: the simulated expected values (probabilities) for the
      specified values of ``x``.

   -  ``qi$pr``: the simulated predicted values for the specified values
      of ``x``.

   -  ``qi$fd``: the simulated first difference in the expected values
      for the values specified in ``x`` and ``x1``.

   -  ``qi$rr``: the simulated risk ratio for the expected values
      simulated from ``x`` and ``x1``.

   -  ``qi$att.ev``: the simulated average expected treatment effect for
      the treated from conditional prediction models.

   -  ``qi$att.pr``: the simulated average predicted treatment effect
      for the treated from conditional prediction models.


See also
+++++

Bayesian probit regression is part of the MCMCpack library by Andrew D.
Martin and Kevin M. Quinn . The convergence diagnostics are part of the
CODA library by Martyn Plummer, Nicky Best, Kate Cowles, and Karen Vines.
