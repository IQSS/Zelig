.. _zlogit:

zelig-logit
~~~~~~

Logistic Regression for Dichotomous Dependent Variables

Logistic regression specifies a dichotomous dependent variable as a
function of a set of explanatory variables.

Syntax
+++++

With reference classes:


.. sourcecode:: r
    

    z5 <- zlogit$new()
    z5$zelig(Y ~ X1 + X ~ X, data = mydata)
    z5$setx()
    z5$sim()


With the Zelig 4 compatibility wrappers:


.. sourcecode:: r
    

    z.out <- zelig(Y ~ X1 + X2, model = "logit", data = mydata)
    x.out <- setx(z.out)
    s.out <- sim(z.out, x = x.out, x1 = NULL)


Examples
+++++



Basic Example
!!!!!

Attaching the sample turnout dataset:


.. sourcecode:: r
    

    data(turnout)


Estimating parameter values for the logistic regression:


.. sourcecode:: r
    

    z.out1 <- zelig(vote ~ age + race, model = "logit", data = turnout)


::

    ## Warning: 'regroup' is deprecated.
    ## Use 'group_by_' instead.
    ## See help("Deprecated")



::

    ## How to cite this model in Zelig:
    ##   Kosuke Imai, Gary King, Olivia Lau. 2007.
    ##   logit: Logistic Regression for Dichotomous Dependent Variables
    ##   in Kosuke Imai, Gary King, and Olivia Lau, "Zelig: Everyone's Statistical Software,"
    ##   http://zeligproject.org/



Setting values for the explanatory variables:


.. sourcecode:: r
    

    x.out1 <- setx(z.out1, age = 36, race = "white")


::

    ## Warning: 'regroup' is deprecated.
    ## Use 'group_by_' instead.
    ## See help("Deprecated")



Simulating quantities of interest from the posterior distribution.


.. sourcecode:: r
    

    s.out1 <- sim(z.out1, x = x.out1)



.. sourcecode:: r
    

    summary(s.out1)


::

    ## 
    ##  sim x :
    ##  -----
    ## ev
    ##           mean         sd       50%      2.5%     97.5%
    ## [1,] 0.7481281 0.01178115 0.7485494 0.7225104 0.7696713
    ## pv
    ##          0     1
    ## [1,] 0.259 0.741




.. sourcecode:: r
    

    plot(s.out1)

.. figure:: figure/Zelig-logit-1-1.png
    :alt: Zelig-logit-1

    Zelig-logit-1

Simulating First Differences
!!!!!

Estimating the risk difference (and risk ratio) between low education
(25th percentile) and high education (75th percentile) while all the
other variables held at their default values.


.. sourcecode:: r
    

    z.out2 <- zelig(vote ~ race + educate, model = "logit", data = turnout)


::

    ## Warning: 'regroup' is deprecated.
    ## Use 'group_by_' instead.
    ## See help("Deprecated")



::

    ## How to cite this model in Zelig:
    ##   Kosuke Imai, Gary King, Olivia Lau. 2007.
    ##   logit: Logistic Regression for Dichotomous Dependent Variables
    ##   in Kosuke Imai, Gary King, and Olivia Lau, "Zelig: Everyone's Statistical Software,"
    ##   http://zeligproject.org/


.. sourcecode:: r
    

    x.high <- setx(z.out2, educate = quantile(turnout$educate, prob = 0.75))


::

    ## Warning: 'regroup' is deprecated.
    ## Use 'group_by_' instead.
    ## See help("Deprecated")


.. sourcecode:: r
    

    x.low <- setx(z.out2, educate = quantile(turnout$educate, prob = 0.25))


::

    ## Warning: 'regroup' is deprecated.
    ## Use 'group_by_' instead.
    ## See help("Deprecated")


.. sourcecode:: r
    

    s.out2 <- sim(z.out2, x = x.high, x1 = x.low)
    summary(s.out2)


::

    ## 
    ##  sim x :
    ##  -----
    ## ev
    ##           mean         sd       50%      2.5%     97.5%
    ## [1,] 0.8223631 0.01069511 0.8224678 0.8011748 0.8429748
    ## pv
    ##          0     1
    ## [1,] 0.185 0.815
    ## 
    ##  sim x1 :
    ##  -----
    ## ev
    ##         mean         sd       50%      2.5%     97.5%
    ## [1,] 0.70984 0.01273342 0.7106361 0.6850838 0.7340319
    ## pv
    ##          0     1
    ## [1,] 0.278 0.722
    ## fd
    ##            mean         sd        50%       2.5%       97.5%
    ## [1,] -0.1125231 0.01143884 -0.1123839 -0.1358231 -0.09090589




.. sourcecode:: r
    

    plot(s.out2)

.. figure:: figure/Zelig-logit-2-1.png
    :alt: Zelig-logit-2

    Zelig-logit-2

Model
+++++

Let :math:`Y_i` be the binary dependent variable for observation
:math:`i` which takes the value of either 0 or 1.

-  The *stochastic component* is given by

   .. math::

      \begin{aligned}
      Y_i &\sim& \textrm{Bernoulli}(y_i \mid \pi_i) \\
          &=& \pi_i^{y_i} (1-\pi_i)^{1-y_i}\end{aligned}

   where :math:`\pi_i=\Pr(Y_i=1)`.

-  The *systematic component* is given by:

   .. math:: \pi_i \; = \; \frac{1}{1 + \exp(-x_i \beta)}.

   where :math:`x_i` is the vector of :math:`k` explanatory variables
   for observation :math:`i` and :math:`\beta` is the vector of
   coefficients.

Quantities of Interest
+++++

-  The expected values (qi$ev) for the logit model are simulations of
   the predicted probability of a success:

   .. math::

      E(Y) =
        \pi_i= \frac{1}{1 + \exp(-x_i \beta)},

   given draws of :math:`\beta` from its sampling distribution.

-  The predicted values (qi$pr) are draws from the Binomial distribution
   with mean equal to the simulated expected value :math:`\pi_i`.

-  The first difference (qi$fd) for the logit model is defined as

   .. math:: \textrm{FD} = \Pr(Y = 1 \mid x_1) - \Pr(Y = 1 \mid x).

-  The risk ratio (qi$rr) is defined as

   .. math:: \textrm{RR} = \Pr(Y = 1 \mid x_1) \ / \ \Pr(Y = 1 \mid x).

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
            \widehat{Y_i(t_i=0)}\right\},

   where :math:`t_i` is a binary explanatory variable defining the
   treatment (:math:`t_i=1`) and control (:math:`t_i=0`) groups.
   Variation in the simulations are due to uncertainty in simulating
   :math:`\widehat{Y_i(t_i=0)}`, the counterfactual predicted value of
   :math:`Y_i` for observations in the treatment group, under the
   assumption that everything stays the same except that the treatment
   indicator is switched to :math:`t_i=0`.

Output Values
+++++

The output of each Zelig command contains useful information which you
may view. For example, if you run
``z.out <- zelig(y ~ x, model = logit, data)``, then you may examine the
available information in ``z.out`` by using ``names(z.out)``, see the
coefficients by using z.out$coefficients, and a default summary of
information through ``summary(z.out)``.

See also
+++++

The logit model is part of the stats package. Advanced users may
wish to refer to ``help(glm)`` and ``help(family)``.
