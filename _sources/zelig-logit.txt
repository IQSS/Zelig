zelig-logit
~~~~~~

Logistic Regression for Dichotomous Dependent Variables

Logistic regression specifies a dichotomous dependent variable as a
function of a set of explanatory variables.

Syntax
+++++


.. sourcecode:: r
    

    z.out <- zelig(Y ~ X1 + X2, model = "logit", data = mydata)
    x.out <- setx(z.out)
    s.out <- sim(z.out, x = x.out, x1 = NULL)


Additional Inputs
+++++

In addition to the standard inputs, zelig() takes the following
additional options for logistic regression:

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
      data frame) The observations are chronologically ordered by the
      size of z.

   -  …: additional options passed to the functions specified in method.
      See the sandwich library and for more options.

Examples
+++++

#. Basic Example

Attaching the sample turnout dataset:


.. sourcecode:: r
    

    data(turnout)


Estimating parameter values for the logistic regression:


.. sourcecode:: r
    

    z.out1 <- zelig(vote   age + race, model = “logit”, data = turnout)


Setting values for the explanatory variables:


.. sourcecode:: r
    

    x.out1 <- setx(z.out1, age = 36, race = “white”)


Simulating quantities of interest from the posterior distribution.


.. sourcecode:: r
    

    s.out1 <- sim(z.out1, x = x.out1)



.. sourcecode:: r
    

    summary(s.out1)



.. sourcecode:: r
    

    plot(s.out1)


#. Simulating First Differences

Estimating the risk difference (and risk ratio) between low education
(25th percentile) and high education (75th percentile) while all the
other variables held at their default values.


.. sourcecode:: r
    

    z.out2 <- zelig(vote   race + educate, model = “logit”, data =
    turnout) > x.high <- setx(z.out2, educate = quantile(turnout\ :math:`educate, prob = 0.75))
    x.low <- setx(z.out2, educate = quantile(turnout`\ educate, prob = 0.25))
    
    s.out2 <- sim(z.out2, x = x.high, x1 = x.low)
    
    summary(s.out2)
    
    plot(s.out2)


#. Presenting Results: An ROC Plot [ROC]

One can use an ROC plot to evaluate the fit of alternative model
specifications. (Use demo(roc) to view this example, or see King and
Zeng (2002).)


.. sourcecode:: r
    

    z.out1 <- zelig(vote   race + educate + age, model = “logit”, +
       data = turnout) > z.out2 <- zelig(vote   race + educate, model =
       “logit”, data = turnout)



.. sourcecode:: r
    

    rocplot(z.out1\ :math:`y, z.out2`\ y, fitted(z.out1),
       fitted(z.out2))



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
