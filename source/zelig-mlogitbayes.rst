.. _zmlogitbayes:

zelig-mlogitbayes
~~~~~

Bayesian Multinomial Logistic Regression

Use Bayesian multinomial logistic regression to model unordered
categorical variables. The dependent variable may be in the format of
either character strings or integer values. The model is estimated via a
random walk Metropolis algorithm or a slice sampler. See for the
maximum-likelihood estimation of this model.

Syntax
+++++

With reference classes:


.. sourcecode:: r
    

    z5 <- zmlogitbayes$new()
    z5$zelig(Y ~ X1 + X2, data = mydata)
    z5$setx()
    z5$sim()


With the Zelig 4 compatibility wrappers:


.. sourcecode:: r
    

    z.out <- zelig(Y ~ X1 + X2, model = "mlogit.bayes", data = mydata)
    x.out <- setx(z.out)
    s.out <- sim(z.out, x = x.out)



Additional Inputs
+++++

zelig() accepts the following arguments for mlogit.bayes:

-  ``baseline``: either a character string or numeric value (equal to
   one of the observed values in the dependent variable) specifying a
   baseline category. The default value is ``NA`` which sets the
   baseline to the first alphabetical or numerical unique value of the
   dependent variable.

The model accepts the following additional arguments to monitor the
Markov chains:

-  ``burnin``: number of the initial MCMC iterations to be discarded
   (defaults to 1,000).

-  ``mcmc``: number of the MCMC iterations after burnin (defaults to
   10,000).

-  ``thin``: thinning interval for the Markov chain. Only every
   ``thin``-th draw from the Markov chain is kept. The value of ``mcmc``
   must be divisible by this value. The default value is 1.

-  ``mcmc.method``: either “MH” or “slice”, specifying whether to use
   Metropolis Algorithm or slice sampler. The default value is ``MH``.

-  ``tune``: tuning parameter for the Metropolis-Hasting step, either a
   scalar or a numeric vector (for :math:`k` coefficients, enter a
   :math:`k` vector). The tuning parameter should be set such that the
   acceptance rate is satisfactory (between 0.2 and 0.5). The default
   value is 1.1.

-  ``verbose``: defaults to ``FALSE``. If ``TRUE``, the progress of the
   sampler (every :math:`10\%`) is printed to the screen.

-  ``seed``: seed for the random number generator. The default is ``NA``
   which corresponds to a random seed of 12345.

-  ``beta.start``: starting values for the Markov chain, either a scalar
   or a vector (for :math:`k` coefficients, enter a :math:`k` vector).
   The default is ``NA`` where the maximum likelihood estimates are used
   as the starting values.

Use the following arguments to specify the priors for the model:

-  ``b0``: prior mean for the coefficients, either a scalar or vector.
   If a scalar, that value will be the prior mean for all the
   coefficients. The default is 0.

-  ``B0``: prior precision parameter for the coefficients, either a
   square matrix with the dimensions equal to the number of coefficients
   or a scalar. If a scalar, that value times an identity matrix will be
   the prior precision parameter. The default is 0 which leads to an
   improper prior.

Zelig users may wish to refer to ``help(MCMCmnl)`` for more information.

Examples
+++++


Basic Example
!!!!!

Attaching the sample dataset:


.. sourcecode:: r
    

    data(mexico)


Estimating multinomial logistics regression using ``mlogit.bayes``:


.. sourcecode:: r
    

    z.out <- zelig(vote88 ~ pristr + othcok + othsocok,
                   model = "mlogit.bayes", data = mexico,
                   verbose = FALSE)


::

    ## Calculating MLEs and large sample var-cov matrix.
    ## This may take a moment...
    ## Inverting Hessian to get large sample var-cov matrix.



::

    ## Warning in if (mcmc.method == "RWM") {: the condition has length > 1 and
    ## only the first element will be used



::

    ## Warning in if (mcmc.method == "IndMH") {: the condition has length > 1 and
    ## only the first element will be used



::

    ## How to cite this model in Zelig:
    ##   Ben Goodrich, Ying Lu. 2013.
    ##   mlogitbayes: Bayesian Multinomial Logistic Regression for Dependent Variables with Unordered Categorical Values
    ##   in Kosuke Imai, Gary King, and Olivia Lau, "Zelig: Everyone's Statistical Software,"
    ##   http://zeligproject.org/



Checking for convergence before summarizing the estimates:


.. sourcecode:: r
    

    raftery.diag(z.out$coefficients)



.. sourcecode:: r
    

    summary(z.out)


Setting values for the explanatory variables to their sample
averages:


.. sourcecode:: r
    

    x.out <- setx(z.out)


Simulating quantities of interest from the posterior distribution
given ``x.out``.


.. sourcecode:: r
    

    s.out1 <- sim(z.out, x = x.out)
    summary(s.out1)


::

    ## 
    ##  sim x :
    ##  -----
    ## ev
    ##             mean         sd       50%      2.5%     97.5%
    ## P(Y=1) 0.5613368 0.01592425 0.5615034 0.5306640 0.5914963
    ## P(Y=2) 0.2099124 0.01273148 0.2098424 0.1854312 0.2350891
    ## P(Y=3) 0.2287508 0.01360126 0.2285987 0.2033590 0.2558153
    ## pv
    ## qi
    ##      1      2      3 
    ## 0.5649 0.2082 0.2269



Simulating First Differences
!!!!!

Estimating the first difference (and risk ratio) in the
probabilities of voting different candidates when ``pristr`` (the
strength of the PRI) is set to be weak (equal to 1) versus strong
(equal to 3) while all the other variables held at their default
values.


.. sourcecode:: r
    

    x.weak <- setx(z.out, pristr = 1)
    x.strong <- setx(z.out, pristr = 3)
    s.out2 <- sim(z.out, x = x.strong, x1 = x.weak)
    summary(s.out2)


::

    ## 
    ##  sim x :
    ##  -----
    ## ev
    ##             mean         sd       50%      2.5%     97.5%
    ## P(Y=1) 0.7156880 0.02127842 0.7158103 0.6725681 0.7561260
    ## P(Y=2) 0.1270237 0.01458077 0.1265905 0.1000858 0.1562571
    ## P(Y=3) 0.1572883 0.01646202 0.1568142 0.1260809 0.1909916
    ## pv
    ## qi
    ##      1      2      3 
    ## 0.7168 0.1230 0.1602 
    ## 
    ##  sim x1 :
    ##  -----
    ## ev
    ##             mean         sd       50%      2.5%     97.5%
    ## P(Y=1) 0.4028126 0.02357831 0.4028038 0.3563194 0.4483880
    ## P(Y=2) 0.3037026 0.02130587 0.3029289 0.2638074 0.3470994
    ## P(Y=3) 0.2934848 0.02189140 0.2931780 0.2517546 0.3372056
    ## pv
    ## qi
    ##      1      2      3 
    ## 0.4066 0.3002 0.2932 
    ## fd
    ##              mean         sd        50%        2.5%      97.5%
    ## P(Y=1) -0.3128754 0.03459857 -0.3128662 -0.38111485 -0.2442630
    ## P(Y=2)  0.1766789 0.02735176  0.1764581  0.12360341  0.2313796
    ## P(Y=3)  0.1361965 0.02881430  0.1363242  0.07966018  0.1935930



Model
+++++

Let :math:`Y_{i}` be the (unordered) categorical dependent variable for
observation :math:`i` which takes an integer values
:math:`j=1, \ldots, J`.

-  The *stochastic component* is given by:

   .. math::

      \begin{aligned}
      Y_{i} &\sim& \textrm{Multinomial}(Y_i \mid \pi_{ij}).\end{aligned}

   where :math:`\pi_{ij}=\Pr(Y_i=j)` for :math:`j=1, \ldots, J`.

-  The *systematic component* is given by

   .. math::

      \begin{aligned}
      \pi_{ij}=\frac{\exp(x_i\beta_j)}{\sum_{k=1}^J \exp(x_i\beta_k)},
      \textrm{ for } j=1,\ldots, J-1,\end{aligned}

   where :math:`x_{i}` is the vector of :math:`k` explanatory variables
   for observation :math:`i` and :math:`\beta_j` is the vector of
   coefficient for category :math:`j`. Category :math:`J` is assumed to
   be the baseline category.

-  The *prior* for :math:`\beta` is given by

   .. math::

      \begin{aligned}
      \beta_j \sim \textrm{Normal}_k\left(  b_{0},B_{0}^{-1}\right) 
      \textrm{ for } j = 1, \ldots, J-1,\end{aligned}

   where :math:`b_{0}` is the vector of means for the :math:`k`
   explanatory variables and :math:`B_{0}` is the :math:`k \times k`
   precision matrix (the inverse of a variance-covariance matrix).

Quantities of Interest
+++++

-  The expected values (``qi$ev``) for the multinomial logistics
   regression model are the predicted probability of belonging to each
   category:

   .. math::

      \begin{aligned}
      \Pr(Y_i=j)=\pi_{ij}=\frac{\exp(x_i \beta_j)}{\sum_{k=1}^J \exp(x_J
      \beta_k)}, \quad \textrm{ for } j=1,\ldots, J-1,\end{aligned}

   and

   .. math::

      \begin{aligned}
      \Pr(Y_i=J)=1-\sum_{j=1}^{J-1}\Pr(Y_i=j)\end{aligned}

   given the posterior draws of :math:`\beta_j` for all categories from
   the MCMC iterations.

-  The predicted values (``qi$pr``) are the draws of :math:`Y_i` from a
   multinomial distribution whose parameters are the expected
   values(\ ``qi$ev``) computed based on the posterior draws of
   :math:`\beta` from the MCMC iterations.

-  The first difference (``qi$fd``) in category :math:`j` for the
   multinomial logistic model is defined as

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
      \frac{1}{n_j}\sum_{i:t_{i}=1}^{n_j}[Y_{i}(t_{i}=1)-E[Y_{i}(t_{i}=0)]],\end{aligned}

   where :math:`t_{i}` is a binary explanatory variable defining the
   treatment (:math:`t_{i}=1`) and control (:math:`t_{i}=0`) groups, and
   :math:`n_j` is the number of treated observations in category
   :math:`j`.

-  In conditional prediction models, the average predicted treatment
   effect (``qi$att.pr``) for the treatment group in category :math:`j`
   is

   .. math::

      \begin{aligned}
      \frac{1}{n_j}\sum_{i:t_{i}=1}^{n_j}[Y_{i}(t_{i}=1)-\widehat{Y_{i}(t_{i}=0)}],\end{aligned}

   where :math:`t_{i}` is a binary explanatory variable defining the
   treatment (:math:`t_{i}=1`) and control (:math:`t_{i}=0`) groups, and
   :math:`n_j` is the number of treated observations in category
   :math:`j`.

Output Values
+++++

The output of each Zelig command contains useful information which you
may view. For example, if you run:


.. sourcecode:: r
    

        z.out <- zelig(y ~ x, model = "mlogit.bayes", data)


then you may examine the available information in ``z.out`` by using
``names(z.out)``, see the draws from the posterior distribution of the
``coefficients`` by using ``z.out$coefficients``, and view a default
summary of information through ``summary(z.out)``. Other elements
available through the ``$`` operator are listed below.


See also
+++++

Bayesian logistic regression is part of the MCMCpack library by Andrew
D. Martin and Kevin M. Quinn . The convergence diagnostics are part of
the CODA library by Martyn Plummer, Nicky Best, Kate Cowles, Karen Vines, Deepayan Sarkar, Russell Almond.
