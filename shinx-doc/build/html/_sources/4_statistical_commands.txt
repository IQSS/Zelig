Statistical Commands
====================

Zelig Commands
--------------

Quick Overview
..............

For any statistical model, Zelig does its work with a combination of three commands.

.. image:: main_commands.png

1. Use ``zelig()`` to run the chosen statistical model on a given data set, with a specific
set of variables. For standard likelihood models, for example, this step estimates the
coefficients, other model parameters, and a variance-covariance matrix. In addition,
you may choose from a variety of options:

  - Pre-process data: Prior to calling ``zelig()``, you may choose from a variety of
    data pre-processing commands (matching or multiple imputation, for example)
    to make your statistical inferences more accurate.

  - Summarize model: After calling ``zelig()``, you may summarize the fitted model
    output using ``summary()``.

  - Validate model: After calling ``zelig()``, you may choose to validate the fitted model. 
    This can be done, for example, by using cross-validation procedures and diagnostics tools.

2. Use ``setx()`` to set each of the explanatory variables to chosen (actual or counterfactual) 
values in preparation for calculating quantities of interest. After calling ``setx()``,
you may use ``WhatIf`` to evaluate these choices by determining whether they involve
interpolation (i.e., are inside the convex hull of the observed data) or extrapolation,
as well as how far these counterfactuals are from the data. Counterfactuals chosen in
``setx()`` that involve extrapolation far from the data can generate considerably more
model dependence (see (37), (39), (48)).

3. Use ``sim()`` to draw simulations of your quantity of interest (such as a predicted value,
predicted probability, risk ratio, or first difference) from the model. (These simulations
may be drawn using an asymptotic normal approximation (the default), bootstrapping,
or other methods when available, such as directly from a Bayesian posterior.) After
calling ``sim()``, use any of the following to summarize the simulations:

  - The ``summary()`` function gives a numerical display. For multiple ``setx()`` values,
    ``summary()`` lets you summarize simulations by choosing one or a subset of observations.

  - If the ``setx()`` values consist of only one observation, ``plot()`` produces density
    plots for each quantity of interest.

Whenever possible, we use ``z.out`` as the ``zelig()`` output object, ``x.out`` as the ``setx()`` output
object, and ``s.out`` as the ``sim()`` output object, but you may choose other names.

Examples
........

- Use the turnout data set included with Zelig to estimate a logit model of an individual’s
  probability of voting as function of race and age. Simulate the predicted probability of
  voting for a white individual, with age held at its mean:


.. sourcecode:: r
    

    data(turnout)
    z.out <- zelig(vote ~ race + age, model = "logit", data = turnout)
    x.out <- setx(z.out, race = "white")
    s.out <- sim(z.out, x = x.out)
    summary(s.out)



- Compute a first difference and risk ratio, changing education from 12 to 16 years, with
  other variables held at their means in the data:


.. sourcecode:: r
    

    data(turnout)
    z.out <- zelig(vote ~ race + educate, model = "logit", data = turnout)
    x.low <- setx(z.out, educate = 12)
    x.high <- setx(z.out, educate = 16)
    s.out <- sim(z.out, x = x.low, x1 = x.high)
    summary(s.out)
    # Numerical summary.
    plot(s.out)



- Calculate expected values for every observation in your data set:


.. sourcecode:: r
    

    data(turnout)
    z.out <- zelig(vote ~ race + educate, model = "logit", data = turnout)
    x.out <- setx(z.out, fn = NULL)
    s.out <- sim(z.out, x = x.out)
    summary(s.out)



- Use five multiply imputed data sets from (47) in an ordered logit model:


.. sourcecode:: r
    

    data(immi1, immi2, immi3, immi4, immi5)
    z.out <- zelig(as.factor(ipip) ~ wage1992 + prtyid + ideol, model = "ologit", 
        data = mi(immi1, immi2, immi3, immi4, immi5))



- Use the nearest propensity score matching via MatchIt package, and then calculate the
  conditional average treatment effect of the job training program based on the linear
  regression model:


.. sourcecode:: r
    

    library(MatchIt)
    data(lalonde)
    m.out <- matchit(treat ~ re74 + re75 + educ + black + hispan + age, data = lalonde, 
        method = "nearest")
    m.data <- match.data(m.out)
    z.out <- zelig(re78 ~ treat + distance + re74 + re75 + educ + black + hispan + 
        age, data = m.data, model = "ls")
    x.out0 <- setx(z.out, fn = NULL, treat = 0)
    x.out1 <- setx(z.out, fn = NULL, treat = 1)
    s.out <- sim(z.out, x = x.out0, x1 = x.out1)
    summary(s.out)



- Validate the fitted model using the leave-one-out cross validation procedure and calculating 
  the average squared prediction error via ``boot`` package. For example:


.. sourcecode:: r
    

    library(boot)
    data(turnout)
    z.out <- zelig(vote ~ race + educate, model = "logit", data = turnout, cite = F)
    cv.out <- cv.glm(z.out, data = turnout, k = 11)
    print(cv.out$delta)



Details
.......

1. ``z.out <- zelig(formula, model, data, by = NULL, ...)``

The ``zelig()`` command estimates a selected statistical model given the specified data.
You may name the output object (``z.out`` above) anything you desire. You must include
three required arguments, in the following order:

  (a) ``formula`` takes the form ``y ~ x1 + x2``, where ``y`` is the dependent variable and ``x1``
      and ``x2`` are the explanatory variables, and ``y``, ``x1``, and ``x2`` are contained in the
      same dataset. The ``+`` symbol means “inclusion” not “addition.” You may include
      interaction terms in the form of ``x1*x2`` without having to compute them in prior
      steps or include the main effects separately. For example, R treats the formula ``y
      ~ x1*x2`` as ``y ~ x1 + x2 + x1*x2``. To prevent R from automatically including
      the separate main effect terms, use the ``I()`` function, thus: ``y ~ I(x1 * x2)``.

  (b) ``model`` lets you choose which statistical model to run. You must put the name
      of the model in quotation marks, in the form ``model = "ls"``, for example. See
      Section ??4.3?? for a list of currently supported models.

  (c) ``data`` specifies the data frame containing the variables called in the formula, in the
      form ``data = mydata``. Alternatively, you may input multiply imputed datasets in
      the form ``data = mi(data1, data2, ...)``. [#]_ If you are working with matched
      data created using ``MatchIt``, you may create a data frame within the ``zelig()``
      statement by using ``data = match.data(...)``. In all cases, the data frame or
      MatchIt object must have been previously loaded into the working memory.

      .. [#] Multiple imputation is a method of dealing with missing values in your data which is more powerful
             than the usual list-wise deletion approach. You can create multiply imputed datasets with a program such
             as Amelia; see King, Honaker, Joseph, Scheve (2000).

  (d) ``by`` (an optional argument which is by default ``NULL``) allows you to choose a factor
      variable (see Section ??2??) in the data frame as a subsetting variable. For each of
      the unique strata defined in the by variable, ``zelig()`` does a separate run of the
      specified model. The variable chosen should not be in the formula, because there
      will be no variance in the by variable in the subsets. If you have one data set for
      all 191 countries in the UN, for example, you may use the by option to run the
      same model 191 times, once on each country, all with a single ``zelig()`` statement.
      You may also use the ``by`` option to run models on ``MatchIt`` subclasses.

  (e) The output object, ``z.out``, contains all of the options chosen, including the name of
      the data set. Because data sets may be large, Zelig does not store the full data set,
      but only the name of the dataset. Every time you use a Zelig function, it looks for
      the dataset with the appropriate name in working memory. (Thus, it is critical
      that you do not change the name of your data set, or perform any additional
      operations on your selected variables between calling ``zelig()`` and ``setx()``, or
      between ``setx()`` and ``sim()``.)

  (f) If you would like to view the regression output at this intermediate step, type
      ``summary(z.out)`` to return the coefficients, standard errors, t-statistics and p-
      values. We recommend instead that you calculate quantities of interest; creating
      ``z.out`` is only the first of three steps in this task.

2. ``x.out <- setx(z.out, fn = list(numeric = mean, ordered = median, others = mode), data = NULL, cond = FALSE, ...)``

  The ``setx()`` command lets you choose values for the explanatory variables, with which
  ``sim()`` will simulate quantities of interest. There are two types of ``setx()`` procedures:

   - You may perform the usual unconditional prediction (by default, ``cond = FALSE``),
    by explicitly choosing the values of each explanatory variable yourself or letting
    ``setx()`` compute them, either from the data used to create ``z.out`` or from a new
    data set specified in the optional data argument. You may also compute predictions
    for all observed values of your explanatory variables using ``fn = NULL``.

  - Alternatively, for advanced uses, you may perform conditional prediction (``cond = TRUE``),
   which predicts certain quantities of interest by conditioning on the observed value of the dependent variable. 
   In a simple linear regression model, this
   procedure is not particularly interesting, since the conditional prediction is merely
   the observed value of the dependent variable for that observation. However, conditional prediction
   is extremely useful for other models and methods, including
   the following:

      * In a matched sampling design, the sample average treatment effect for the
        treated can be estimated by computing the difference between the observed
        dependent variable for the treated group and their expected or predicted
        values of the dependent variable under no treatment (16).

      * With censored data, conditional prediction will ensure that all predicted values
        are greater than the censored observed values (28).

      * In ecological inference models, conditional prediction guarantees that the predicted values are on the tomography line and 
        thus``.




