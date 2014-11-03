.. _zoprobitbayes:

zelig-oprobitbayes
~~~~~~

Bayesian Ordered Probit Regression

Use the ordinal probit regression model if your dependent variables are
ordered and categorical. They may take either integer values or
character strings. The model is estimated using a Gibbs sampler with
data augmentation. For a maximum-likelihood implementation of this
models, see `probit`.

Syntax
+++++

With reference classes:


.. sourcecode:: r
    

    z5 <- zoprobitbayes$new()
    z5$zelig(Y ~ X1 + X2, data = mydata)
    z5$setx()
    z5$sim()


With the Zelig 4 compatibility wrappers:


.. sourcecode:: r
    

    z.out <- zelig(Y ~ X1 + X2, model = "oprobit.bayes", data = mydata)
    x.out <- setx(z.out)
    s.out <- sim(z.out, x = x.out)


Additional Inputs
+++++

zelig() accepts the following arguments to monitor the Markov chain:

-  ``burnin``: number of the initial MCMC iterations to be discarded
   (defaults to 1,000).

-  ``mcmc``: number of the MCMC iterations after burnin (defaults
   10,000).

-  ``thin``: thinning interval for the Markov chain. Only every
   ``thin``-th draw from the Markov chain is kept. The value of ``mcmc``
   must be divisible by this value. The default value is 1.

-  ``tune``: tuning parameter for the Metropolis-Hasting step. The
   default value is ``NA`` which corresponds to 0.05 divided by the
   number of categories in the response variable.

-  ``verbose``: defaults to FALSE If ``TRUE``, the progress of the
   sampler (every :math:`10\%`) is printed to the screen.

-  ``seed``: seed for the random number generator. The default is ``NA``
   which corresponds to a random seed 12345.

-  ``beta.start``: starting values for the Markov chain, either a scalar
   or vector with length equal to the number of estimated coefficients.
   The default is ``NA``, which uses the maximum likelihood estimates as
   the starting values.

Use the following parameters to specify the model’s priors:

-  ``b0``: prior mean for the coefficients, either a numeric vector or a
   scalar. If a scalar value, that value will be the prior mean for all
   the coefficients. The default is 0.

-  ``B0``: prior precision parameter for the coefficients, either a
   square matrix (with dimensions equal to the number of coefficients)
   or a scalar. If a scalar value, that value times an identity matrix
   will be the prior precision parameter. The default is 0 which leads
   to an improper prior.

Zelig users may wish to refer to ``help(MCMCoprobit)`` for more
information.

Examples
+++++



Basic Example
!!!!!

Attaching the sample dataset:


.. sourcecode:: r
    

    data(sanction)


Estimating ordered probit regression using ``oprobit.bayes``:


.. sourcecode:: r
    

    z.out <- zelig(ncost ~ mil + coop, model = "oprobit.bayes",
                   data = sanction, verbose = FALSE)


::

    ## Warning in model.response(mf, "numeric"): using type = "numeric" with a
    ## factor response will be ignored



::

    ## How to cite this model in Zelig:
    ##   Ben Goodrich, Ying Lu. 2013.
    ##   oprobitbayes: Bayesian Probit Regression for Dichotomous Dependent Variables
    ##   in Kosuke Imai, Gary King, and Olivia Lau, "Zelig: Everyone's Statistical Software,"
    ##   http://zeligproject.org/



Creating an ordered dependent variable:


.. sourcecode:: r
    

    sanction$ncost <- factor(sanction ~ ncost, ordered = TRUE,
                             levels = c("net gain", "little effect", "modest loss",
                                         "major loss"))


::

    ## Error in as.vector(x, mode): invalid 'mode' argument



Checking for convergence before summarizing the estimates:


.. sourcecode:: r
    

    heidel.diag(z.out$coefficients)
    raftery.diag(z.out$coefficients)



.. sourcecode:: r
    

    summary(z.out)


Setting values for the explanatory variables to their sample averages:


.. sourcecode:: r
    

    x.out <- setx(z.out)


Simulating quantities of interest from the posterior distribution given: ``x.out``.


.. sourcecode:: r
    

    s.out1 <- sim(z.out, x = x.out)
    summary(s.out1)


::

    ## 
    ##  sim x :
    ##  -----
    ## ev
    ##                     mean         sd        50%       2.5%      97.5%
    ## little effect 0.44981581 0.05601155 0.44883053 0.34151148 0.56103192
    ## major loss    0.04473004 0.02101827 0.04176495 0.01271310 0.09412959
    ## modest loss   0.12341501 0.03950140 0.11984735 0.06005967 0.22834556
    ## net gain      0.38203914 0.05548348 0.38097907 0.27602445 0.49205957
    ## pv
    ## qi
    ## little effect    major loss   modest loss      net gain 
    ##        0.1868        0.2731        0.5213        0.0188



Simulating First Differences
!!!!!

Estimating the first difference (and risk ratio) in the probabilities
of incurring different level of cost when there is no military action
versus military action while all the other variables held at their default values.


.. sourcecode:: r
    

    x.high <- setx(z.out, mil = 0)
    x.low <- setx(z.out, mil = 1)



.. sourcecode:: r
    

    s.out2 <- sim(z.out, x = x.high, x1 = x.low)
    summary(s.out2)


::

    ## 
    ##  sim x :
    ##  -----
    ## ev
    ##                     mean         sd        50%       2.5%      97.5%
    ## little effect 0.43844669 0.05843957 0.43767439 0.32758262 0.55410854
    ## major loss    0.04458012 0.02095061 0.04165022 0.01271633 0.09387773
    ## modest loss   0.12377654 0.03963082 0.12024928 0.05993637 0.22927426
    ## net gain      0.39319665 0.05795514 0.39196529 0.28336346 0.50825527
    ## pv
    ## qi
    ## little effect    major loss   modest loss      net gain 
    ##        0.1491        0.2382        0.5780        0.0347 
    ## 
    ##  sim x1 :
    ##  -----
    ## ev
    ##                    mean         sd        50%       2.5%      97.5%
    ## little effect 0.5464229 0.16109327 0.54796938 0.23474072 0.84451223
    ## major loss    0.0407613 0.01998891 0.03763614 0.01085385 0.08828575
    ## modest loss   0.1075956 0.03975880 0.10421634 0.04208043 0.20132712
    ## net gain      0.3052203 0.14485132 0.29018143 0.07362204 0.62315150
    ## pv
    ## qi
    ## little effect    major loss   modest loss      net gain 
    ##        0.6116        0.0963        0.1862        0.1059 
    ## fd
    ##                       mean         sd          50%        2.5%       97.5%
    ## little effect  0.107976200 0.17020693  0.111082214 -0.22740084 0.426282862
    ## major loss    -0.003818825 0.00665593 -0.001418880 -0.02253975 0.002180789
    ## modest loss   -0.016180976 0.02172754 -0.008327805 -0.07580491 0.005015329
    ## net gain      -0.087976398 0.15275950 -0.102164237 -0.34480882 0.241227653



Model
+++++

Let :math:`Y_{i}` be the ordered categorical dependent variable for
observation :math:`i` which takes an integer value
:math:`j=1, \ldots, J`.

-  The *stochastic component* is described by an unobserved continuous
   variable, :math:`Y_i^*`,

   .. math::

      \begin{aligned}
      Y_{i}^*  \sim \textrm{Normal}(\mu_i, 1).\end{aligned}

   Instead of :math:`Y_i^*`, we observe categorical variable
   :math:`Y_i`,

   .. math::

      \begin{aligned}
      Y_i = j \quad \textrm{ if } \tau_{j-1} \le Y_i^* \le \tau_j \textrm{
      for } j=1,\ldots, J.\end{aligned}

   where :math:`\tau_j` for :math:`j=0,\ldots, J` are the threshold
   parameters with the following constraints, :math:`\tau_l < \tau_m`
   for :math:`l < m`, and :math:`\tau_0=-\infty, \tau_J=\infty`.

   The probability of observing :math:`Y_i` equal to category :math:`j`
   is,

   .. math::

      \begin{aligned}
      \Pr(Y_i=j) &=& \Phi(\tau_j \mid \mu_i)-\Phi(\tau_{j-1} \mid \mu_i) 
      \textrm{ for } j=1,\ldots, J\end{aligned}

   where :math:`\Phi(\cdot \mid \mu_i)` is the cumulative distribution
   function of the Normal distribution with mean :math:`\mu_i` and
   variance 1.

-  The *systematic component* is given by

   .. math::

      \begin{aligned}
      \mu_{i}= x_i \beta,\end{aligned}

   where :math:`x_{i}` is the vector of :math:`k` explanatory variables
   for observation :math:`i` and :math:`\beta` is the vector of
   coefficients.

-  The *prior* for :math:`\beta` is given by

   .. math::

      \begin{aligned}
      \beta \sim \textrm{Normal}_k\left(  b_{0},B_{0}^{-1}\right)\end{aligned}

   where :math:`b_{0}` is the vector of means for the :math:`k`
   explanatory variables and :math:`B_{0}` is the :math:`k \times k`
   precision matrix (the inverse of a variance-covariance matrix).

Quantities of Interest
+++++

-  The expected values (``qi$ev``) for the ordered probit model are the
   predicted probability of belonging to each category:

   .. math::

      \begin{aligned}
      \Pr(Y_i=j)= \Phi(\tau_j \mid x_i \beta)-\Phi(\tau_{j-1} \mid x_i \beta),\end{aligned}

   given the posterior draws of :math:`\beta` and threshold parameters
   :math:`\tau` from the MCMC iterations.

-  The predicted values (``qi$pr``) are the observed values of
   :math:`Y_i` given the observation scheme and the posterior draws of
   :math:`\beta` and cut points :math:`\tau` from the MCMC iterations.

-  The first difference (``qi$fd``) in category :math:`j` for the
   ordered probit model is defined as

   .. math::

      \begin{aligned}
      \text{FD}_j=\Pr(Y_i=j\mid X_{1})-\Pr(Y_i=j\mid X).\end{aligned}

-  The risk ratio (``qi$rr``) in category :math:`j` is defined as

   .. math::

      \begin{aligned}
      \text{RR}_j=\Pr(Y_i=j\mid X_{1})\ /\ \Pr(Y_i=j\mid X).\end{aligned}

-  In conditional prediction models, the average expected treatment
   effect (``qi$att.ev``) for the treatment group in category :math:`j`
   is

   .. math::

      \begin{aligned}
      \frac{1}{n_j}\sum_{i:t_{i}=1}^{n_j} \{
      Y_{i}(t_{i}=1)-E[Y_{i}(t_{i}=0)] \},\end{aligned}

   where :math:`t_{i}` is a binary explanatory variable defining the
   treatment (:math:`t_{i}=1`) and control (:math:`t_{i}=0`) groups, and
   :math:`n_j` is the number of observations in the treatment group that
   belong to category :math:`j`.

-  In conditional prediction models, the average predicted treatment
   effect (``qi$att.pr``) for the treatment group in category :math:`j`
   is

   .. math::

      \begin{aligned}
      \frac{1}{n_j}\sum_{i:t_{i}=1}^{n_j}[Y_{i}(t_{i}=1)-\widehat{Y_{i}(t_{i}=0)}],\end{aligned}

   where :math:`t_{i}` is a binary explanatory variable defining the
   treatment (:math:`t_{i}=1`) and control (:math:`t_{i}=0`) groups, and
   :math:`n_j` is the number of observations in the treatment group that
   belong to category :math:`j`.

Output Values
+++++

The output of each Zelig command contains useful information which you
may view. For example, if you run:

::

    z.out <- zelig(y ~ x, model = "oprobit.bayes", data)

then you may examine the available information in ``z.out`` by using
``names(z.out)``, see the draws from the posterior distribution of the
``coefficients`` by using ``z.out$coefficients``, and view a default
summary of information through ``summary(z.out)``. Other elements
available through the ``$`` operator are listed below.

-  From the ``zelig()`` output object ``z.out``, you may extract:

   -  ``coefficients``: draws from the posterior distributions of the
      estimated coefficients :math:`\beta` and threshold parameters
      :math:`\tau`. Note, element :math:`\tau_1` is normalized to 0 and
      is not returned in the ``coefficients`` object.

   -  zelig.data: the input data frame if save.data = TRUE.

   -  ``seed``: the random seed used in the model.

-  From the ``sim()`` output object ``s.out``:

   -  ``qi$ev``: the simulated expected values (probabilities) of each
      of the :math:`J` categories for the specified values of ``x``.

   -  ``qi$pr``: the simulated predicted values (observed values) for
      the specified values of ``x``.

   -  ``qi$fd``: the simulated first difference in the expected values
      of each of the :math:`J` categories for the values specified in
      ``x`` and ``x1``.

   -  ``qi$rr``: the simulated risk ratio for the expected values of
      each of the :math:`J` categories simulated from ``x`` and ``x1``.

   -  ``qi$att.ev``: the simulated average expected treatment effect for
      the treated from conditional prediction models.

   -  ``qi$att.pr``: the simulated average predicted treatment effect
      for the treated from conditional prediction models.

See also
+++++

Bayesian ordinal probit regression is part of the MCMCpack library by
Andrew D. Martin and Kevin M. Quinn . The convergence diagnostics are
part of the CODA library by Martyn Plummer, Nicky Best, Kate Cowles, and
Karen Vines.
