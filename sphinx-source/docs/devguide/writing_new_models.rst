Writing New Models
==================

With Zelig, writing a new model in R is straightforward. (If you already
have a model, see Chapter [c:addingmodels] for how to include it in
Zelig.) With tools to streamline user inputs, writing a new model does
not require a lot of programming knowledge, but lets developers focus on
the model’s math. Generally, writing a new statistical procedure or
model comes in orderly steps:

#. Write down the mathematical model. Define the parameters that you
   need, grouping parameters into convenient vectors or matrices
   whenever possible (this will make your code clearer).

#. Write the code.

#. Test the code (usually using Monte Carlo data, where you know the
   true values being estimated ) and make sure that it works as
   expected.

#. Write some documentation explaining your model and the functions that
   run your model.

Somewhere between steps [1] and [2], you will need to translate input
data into the mathematical notation that you used to write down the
model. Rather than repeating whole blocks of code, use functions to
streamline the number of commands that users will need to run your
model.

With more steps being performed by fewer commands, the inputs to these
commands become more sophisticated. The structure of those inputs
actually matters quite a lot. If your function has a convoluted syntax,
it will be difficult to use, difficult to explain, and difficult to
document. If your function is easy to use and has an intuitive syntax,
however, it will be easy to explain and document, which will make your
procedure more accessible to all users.

Managing Statistical Model Inputs
---------------------------------

Most statistical models require a matrix of explanatory variables and a
matrix of dependent variables. Rather than have users create matrices
themselves, R has a convenient user interface to create matrices of
response and explanatory variables on the fly. Users simply specify a
formula in the form of ``dependent ~ explanatory variables``, and
developers use the following functions to transform the formula into the
appropriate matrices. Let mydata be a data frame.

.. sourcecode:: r

    > formula <- y ~ x1 + x2                   # User input

    # Given the formula above, programmers can use the following standard commands
    > D <- model.frame(formula, data = mydata) # Subset & listwise deletion
    > X <- model.matrix(formula, data = D)     # Creates X matrix
    > Y <- model.response(D)                   # Creates Y matrix

where

-  D is a subset of mydata that contains only the variables specified in
   the formula (y, x1, and x2) with listwise deletion performed on the
   subset data frame;

-  X is a matrix that contains a column of 1’s, and the explanatory
   variables x1 and x2 from D; and

-  Y is a matrix containing the dependent variable(s) from D.

Depending on the model, Y may be a column vector, matrix, or other data
structure.

Describe the Statistical Model
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

After setting up the :math:`X` matrix, the next step for most models
will be to identify the corresponding vector of parameters. For a single
response variable model with no ancillary parameters, the standard R
interface is quite convenient: given :math:`X`, the model’s parameters
are simply :math:`\beta`.

There are very few models, however, that fall into this category. Even
Normal regression, for example, has two sets of parameters :math:`\beta`
and :math:`\sigma^2`. In order to make the R formula format more
flexible, Zelig has an additional set of tools that lets you describe
the inputs to your model (for multiple sets of parameters).

After you have written down the statistical model, identify the
parameters in your model. With these parameters in mind, the first step
is to write a describe.\*() function for your model. If your model is
called mymodel, then the describe.mymodel() function takes no arguments
and returns a list with the following information:

-  category: a character string that describes the dependent variable.
   See for the current list of available categories.

-  parameters: a list containing parameter sets used in your model. For
   each parameter (e.g., theta), you need to provide the following
   information:

   -  equations: an integer number of equations for the parameter. For
      parameters that can take, for example, two to four equations, use
      c(2, 4).

   -  tagsAllowed: a logical value (TRUE/FALSE) specifying whether a
      given parameter allows constraints.

   -  depVar: a logical value (TRUE/FALSE) specifying whether a
      parameter requires a corresponding dependent variable.

   -  expVar: a logical value (TRUE/FALSE) specifying whether a
      parameter allows explanatory variables.

(See for examples and additional arguments output by
describe.mymodel().)

Single Response Variable Models: Normal Regression Model
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let’s say that you are trying to write a Normal regression model with
stochastic component

.. math::

   \textrm{Normal}(y_i \mid \mu_i, \sigma^2) \; = \; \frac{1}{\sqrt{2 \pi} \sigma} \, \exp
   \left( -\left( \frac{(y_i - \mu_i)^2}{2 \sigma^2} \right) \right)

with scalar variance parameter :math:`\sigma^2 > 0`, and systematic
component :math:`E(Y_i) = \mu_i = x_i \beta`. This implies two sets of
parameters in your model, and the following describe.normal.regression()
function:

.. sourcecode:: r

    describe.normal.regression <- function() {
      category <- "continuous"
      mu <- list(equations = 1,              # Systematic component
                 tagsAllowed = FALSE,
                 depVar = TRUE,
                 expVar = TRUE)
      sigma2 <- list(equations = 1,          # Scalar ancillary parameter
                     tagsAllowed = FALSE,
                     depVar = FALSE,
                     expVar = FALSE)
      pars <- list(mu = mu, sigma2 = sigma2)
      list(category = category, parameters = pars)
    }

To find the log-likelihood:

.. math::

   \begin{aligned}
   \textrm{L }(\beta, \sigma^2 \mid y) & = & \prod_{1=1}^n
          \textrm{Normal}(y_i \mid \mu_i, \sigma^2)\\
       & = & \prod_{i=1}^n (2\pi\sigma^2)^{-1/2}\exp\left(\frac{-(y_i-\mu_i)^2}
   {2\sigma^2}\right)\\
       & = &(2\pi\sigma^2)^{-n/2} \prod_{i=1}^n \exp\left(\frac{-(y_i-\mu_i)^2}
   {2\sigma^2}\right)\\
       & = &(2\pi\sigma^2)^{-n/2} \prod_{i=1}^n \exp\left(\frac{-(y_i-x_i \beta)^2}
   {2\sigma^2}\right)\\
   \ln \textrm{L }(\beta, \sigma^2 \mid y) &=& -\frac{n}{2}\ln(2\pi\sigma^2)-
   \sum_{i=1}^n \frac{(y_i-x_i\beta)^2}{2\sigma^2}\\
          &=& -\frac{n}{2}\ln(2\pi\sigma^2)-
           \frac{1}{2\sigma^2}\sum_{i=1}^n (y_i-x_i\beta)^2 \\
   &\propto& -\frac{1}{2} \left( n \ln\sigma^2 + \frac{\sum_{i=1}^n
   (y_i-x_i\beta)^2}{\sigma^2} \right)\end{aligned}

In R code, this translates to:

.. sourcecode:: r

    ll.normal <- function(par, X, Y, n, terms) {
      beta <- parse.par(par, terms, eqn = "mu")             # [1]
      gamma <- parse.par(par, terms, eqn = "sigma2")        # [2]
      sigma2 <- exp(gamma)
      -0.5 * (n * log(sigma2) + sum((Y - X %*% beta)^2 / sigma2))
    }

At Comment [1] above, we use the function parse.par() to pull out the
vector of parameters beta (which relate the systematic component
:math:`\mu_i` to the explanatory variables :math:`x_i`). No matter how
many covariates there are, the parse.par() function can use terms to
pull out the appropriate parameters from par. We also use parse.par() at
Comment [2] to pull out the scalar ancillary parameter that (after
transformation) corresponds to the :math:`\sigma^2` parameter.

To optimize this function, simply type:

.. sourcecode:: r

    out <- optim(start.val, ll.normal, control = list(fnscale = -1),
                 method = "BFGS", hessian = TRUE, X = X, Y = Y, terms = terms)

where

-  start.val is a vector of starting values for par. Use set.start() to
   create starting values for all parameters, systematic and ancillary,
   in one step.

-  ll.normal is the log-likelihood function derived above.

-  “BFGS” specifies unconstrained optimization using a quasi-Newton
   method.

-  control = list(fnscale = -1) specifies that R should maximize the
   function (omitting this causes R to minimize the function by
   default).

-  hessian = TRUE instructs R to return the Hessian matrix (from which
   you may calculate the variance-covariance matrix).

-  X and Y are the matrix of explanatory variables and vector of
   dependent variables, used in the ll.normal() function.

-  terms are meta-data constructed from the model.frame() command.

Please refer to the R-help for optim() for more options.

To make this procedure generalizable, we can write a function that takes
a user-specified data frame and formula, and optional starting values
for the optimization procedure:

.. sourcecode:: r

    normal.regression <- function(formula, data, start.val = NULL, ...) {

      fml <- parse.formula(formula, model = "normal.regression") # [1]
      D <- model.frame(fml, data = data)
      X <- model.matrix(fml, data = D)
      Y <- model.response(D)
      terms <- attr(D, "terms")
      n <- nrow(X)

      start.val <- set.start(start.val, terms)

      res <- optim(start.val, ll.normal, method = "BFGS",        
                   hessian = TRUE, control = list(fnscale = -1),
                   X = X, Y = Y, n = n, terms = terms, ...)      # [2]

      fit <- model.end(res, D)                                   # [3]
      fit$n <- n
      class(fit) <- "normal"                                     # [4]
      fit                                                        
    }

The following comments correspond to the bracketed numbers above:

#. The parse.formula() command looks for the
   describe.normal.regression() function, which changes the
   user-specified formula into the following format:

   .. sourcecode:: r

       list(mu = formula,         # where `formula' was specified by the user
            sigma = ~ 1)

#. The … here indicate that if the user enters any additional arguments
   when calling normal.regression(), that those arguments should go to
   the optim() function.

#. The model.end() function takes the optimized output and the listwise
   deleted data frame D and creates an object that will work with
   setx().

#. Choose a class for your model output so that you will be able to
   write an appropriate summary(), param(), and qi() function for your
   model.

Multivariate models: Bivariate Normal example
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Most common models have one systematic component. For :math:`n`
observations, the systematic component varies over observations
:math:`i = 1,
\dots, n`. In the case of the Normal regression model, the systematic
component is :math:`\mu_i` (:math:`\sigma^2` is not estimated as a
function of covariates).

In some cases, however, your model may have more than one systematic
component. In the case of bivariate probit, we have a dependent variable
:math:`Y_i = (Y_{i1}, Y_{i2})` observed as (0,0), (1,0), (0,1), or (1,1)
for :math:`i = 1, \dots, n`. Similar to a single-response probit model,
the stochastic component is described by two latent (unobserved)
continuous variables (:math:`Y_{i1}^*`, :math:`Y_{i2}^*`) which follow
the bivariate Normal distribution:

.. math::

   \begin{aligned}
     \left ( \begin{array}{c} 
         Y_{i1}^* \\
         Y_{i2}^* 
       \end{array}
     \right ) &\sim &  
     \textrm{Normal} \left \{ \left ( 
         \begin{array}{c}
           \mu_{i1} \\ \mu_{i2}
         \end{array} \right ), \left( \begin{array}{cc}
                    1 & \rho \\
                    \rho & 1 
                    \end{array} \right) \right\},\end{aligned}

where for :math:`j = 1, 2`, :math:`\mu_{ij}` is the mean for
:math:`Y_{ij}^*` and :math:`\rho` is a correlation parameter. The
following observation mechanism links the observed dependent variables,
:math:`Y_{ij}`, with these latent variables

.. math::

   \begin{aligned}
   Y_{ij} & = & \left \{ \begin{array}{cc}
                      1 & {\rm if} \; Y_{ij}^* \ge 0, \\
                      0 & {\rm otherwise.}
                      \end{array} 
                      \right.\end{aligned}

The systemic components for each observation are

.. math::

   \begin{aligned}
       \mu_{ij} & = & x_{ij} \beta_j \quad {\rm for} \quad j = 1, 2, \\
       \rho & = & \frac{\exp(x_{i3} \beta_3) - 1}{\exp(x_{i3} \beta_3) + 1}.\end{aligned}

In the default specification, :math:`\rho` is a scalar (such that
:math:`x_{i3}` only contains an intercept term).

If so, we have two sets of parameters: :math:`\mu_{i} = (\mu_{i1},
\mu_{i2})` and :math:`\rho`. This implies the following
describe.bivariate.probit() function:

.. sourcecode:: r

    describe.bivariate.probit <- function() {
      category <- "dichotomous"
      package <- list(name = "mvtnorm",       # Required package and 
                      version = "0.7")        #  minimum version number
      mu <- list(equations = 2,               # Systematic component has 2
                 tagsAllowed = TRUE,          #  required equations
                 depVar = TRUE, 
                 expVar = TRUE), 
      rho <- list(equations = 1,              # Optional systematic component
                 tagsAllowed = FALSE,         #   (estimated as an ancillary
                 depVar = FALSE,              #    parameter by default)
                 expVar = TRUE), 
      pars <- parameters(mu = mu, rho = rho)
      list(category = category, package = package, parameters = pars)
    }

Since users may choose different explanatory variables to parameterize
:math:`\mu_{i1}` and :math:`\mu_{i2}` (and sometimes :math:`\rho`), the
model requires a minimum of *two* formulas. For example,

.. sourcecode:: r

    formulae <- list(mu1 = y1 ~ x1 + x2,                         # User input
                     mu2 = y2 ~ x2 + x3)
    fml <- parse.formula(formulae, model = "bivariate.probit")   # [1]
    D <- model.frame(fml, data = mydata)
    X <- model.matrix(fml, data = D)
    Y <- model.response(D)

At comment [1], parse.formula() finds the describe.bivariate.probit()
function and parses the formulas accordingly.

If :math:`\rho` takes covariates (and becomes a systematic component
rather than an ancillary parameter), there can be three sets of
explanatory variables:

.. sourcecode:: r

    formulae <- list(mu1 = y1 ~ x1 + x2, 
                     mu2 = y2 ~ x2 + x3, 
                     rho = ~ x4 + x5) 

From the perspective of the programmer, a nearly identical framework
works for both single and multiple equation models. The
(parse.formula()) line changes the class of fml from “list” to
“multiple” and hence ensures that model.frame() and model.matrix() go to
the appropriate methods. D, X , and Y are analogous to their single
equation counterparts above:

-  D is the subset of mydata containing the variables y1, y2, x1, x2,
   and x3 with listwise deletion performed on the subset;

-  X is a matrix corresponding to the explanatory variables, in one of
   three forms discussed below (see ).

-  Y is an :math:`n \times J` matrix (where :math:`J=2` here) with
   columns (y1, y2) corresponding to the outcome variables on the
   left-hand sides of the formulas.

Given for the bivariate probit probability density described above, the
likelihood is:

.. math::

   L(\mathbf{\pi} | Y_i) = \prod_{i=1}^n 
                       \pi_{00}^{\textrm{I}\{Y_i = (0,0)\}}
                       \pi_{10}^{\textrm{I}\{Y_i = (1,0)\}}
                       \pi_{01}^{\textrm{I}\{Y_i = (0,1)\}}
                       \pi_{11}^{\textrm{I}\{Y_i = (1,1)\}}

where I is an indicator function and

-  :math:`\pi_{00} = \int_{-\infty}^0 \int_{-\infty}^0 \textrm{Normal}(Y_{i1}^*, Y_{i2}^* \mid
   \mu_{i1}, \mu_{i2}, \rho) dY_{i2}^* dY_{i1}^*`

-  :math:`\pi_{10} = \int_0^{\infty} \int_{-\infty}^0 \textrm{Normal}(Y_{i1}^*, Y_{i2}^* \mid
   \mu_{i1}, \mu_{i2}, \rho) dY_{i2}^* dY_{i1}^*`

-  :math:`\pi_{01} = \int_{-\infty}^0 \int_0^{\infty} \textrm{Normal}(Y_{i1}^*, Y_{i2}^* \mid
   \mu_{i1}, \mu_{i2}, \rho) dY_{i2}^* dY_{i1}^*`

-  :math:`\pi_{11} = 1-\pi_{00}-\pi_{10}-\pi_{01}`

This implies the following log-likelihood:

.. math::

   \begin{aligned}
   \log L(\mathbf{\pi} | Y_i) &=& \sum_{i=1}^n \textrm{I}\{Y_i = (0,0)\} \log\pi_{00}
   + \textrm{I}\{Y_i = (1,0)\} \log \pi_{10} \\
   && \quad \quad + \textrm{I}\{Y_i = (0,1)\} \log \pi_{01}
   + \textrm{I}\{Y_i = (1,1)\} \log \pi_{11}\end{aligned}

(For the corresponding R code, see below.)

Easy Ways to Manage Matrices
----------------------------

Most statistical methods relate explanatory variables :math:`x_i` to a
dependent variable of interest :math:`y_i` for each observation
:math:`i = 1,
\dots, n`. Let :math:`\beta` be a set of parameters that correspond to
each column in :math:`X`, which is an :math:`n \times k` matrix with
rows :math:`x_i`. For a single equation model, the linear predictor is

.. math::

   \eta_i = x_i \beta = \beta_0 + \beta_1 x_{i1} +
   \beta_2 x_{i2} + \dots + \beta_k x_{ik}

Thus, :math:`\eta` is the set of :math:`\eta_i` for
:math:`i = 1, \dots, n` and is usually represented as an
:math:`n \times 1` matrix.

For a two equation model such as bivariate probit, the linear predictor
becomes a matrix with columns corresponding to each dependent variable
:math:`(y_{1i}, y_{2i})`:

.. math:: \eta_i = (\eta_{i1}, \eta_{i2}) = (x_{i1} \beta_1, x_{i2} \beta_2)

With :math:`\eta` as an :math:`n \times 2` matrix, we now have a few
choices as to how to create the linear predictor:

#. An **intuitive** layout, which stacks matrices of explanatory
   variables, provides an easy visual representation of the relationship
   between explanatory variables and coefficients;

#. A **computationally-efficient** layout, which takes advantage of
   computational vectorization; and

#. A **memory-saving** layout, which reduces the overall size of the
   :math:`X` and :math:`\beta` matrices.

Using the simple tools described in this section, you can pick the best
matrix management method for your model.

In addition, the way in which :math:`\eta` is created also affects the
way parameters are estimated. Let’s say that you want two parameters to
have the same effect in different equations. By setting up :math:`X` and
:math:`\beta` in a certain way, you can let users set constraints across
parameters. Continuing the bivariate probit example above, let the model
specification be:

.. sourcecode:: r

    formulae <- list(mu1 = y1 ~ x1 + x2 + tag(x3, "land"), 
                     mu2 = y2 ~ x3 + tag(x4, "land"))

where tag() is a special function that constrains variables to have the
same effect across equations. Thus, the coefficient for x3 in equation
mu1 is constrained to be equal to the coefficient for x4 in equation
mu2, and this effect is identified as the “land” effect in both
equations. In order to consider constraints across equations, the
structure of both :math:`X` and :math:`\beta` matter.

The Intuitive Layout
~~~~~~~~~~~~~~~~~~~~

A stacked matrix of :math:`X` and vector :math:`\beta` is probably the
most visually intuitive configuration. Let :math:`J = 2` be the number
of equations in the bivariate probit model, and let :math:`v_t` be the
total number of unique covariates in both equations. Choosing
model.matrix(…, shape = “stacked”) yields a :math:`(Jn \times v_t) =
(2n \times 6)` matrix of explanatory variables. Again, let :math:`x_1`
be an :math:`n \times 1` vector representing variable x1, :math:`x_2`
x2, and so forth. Then

.. math::

   X = \left (\begin{array}{cccccc}
   1 & 0 & x_1 & x_2 & 0   & x_3  \\ 
   0 & 1 & 0   & 0   & x_3 & x_4
   \end{array} \right)

Correspondingly, :math:`\beta` is a vector with elements

.. math::

   (\beta_0^{\mu_1} \; \beta_0^{\mu_2} \; \beta_{x_1}^{\mu_1} \;
   \beta_{x_2}^{\mu_1} \; \beta_{x_3}^{\mu_2} \; \beta_{\textrm{land}})\prime

where :math:`\beta_0^j` are the intercept terms for equation
:math:`j = \{\mu_1,
\mu_2\}`. Since :math:`X` is :math:`(2n \times 6)` and :math:`\beta` is
:math:`(6 \times 1)`, the resulting linear predictor :math:`\eta` is
also stacked into a :math:`(2n \times
1)` matrix. Although difficult to manipulate (since observations are
indexed by :math:`i` and :math:`2i` for each :math:`i = 1, \dots, n`
rather than just :math:`i`), it is easy to see that we have turned the
two equations into one big :math:`X` matrix and one long vector
:math:`\beta`, which is directly analogous to the familiar
single-equation :math:`\eta`.

The Computationally-Efficient Layout
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Choosing array :math:`X` and vector :math:`\beta` is probably the the
most computationally-efficient configuration: model.matrix(…, shape =
“array”) produces an :math:`n \times k_t
\times J` array where :math:`J` is the total number of equations and
:math:`k_t` is the total number of parameters across all the equations.
Since some parameter values may be constrained across equations,
:math:`k_t \leq
\sum_{j=1}^J k_j`. If a variable is not in a certain equation, it is
observed as a vector of 0s. With this option, each
:math:`i = 1, \dots, n` :math:`x_i` matrix becomes:

.. math::

   \left( \begin{array}{ccccccc}
   1 & 0 & x_{i1} & x_{i2} & 0      & x_{i3} \\
   0 & 1 & 0      & 0      & x_{i3} & x_{i4}
   \end{array} \right)

By stacking each of these :math:`x_i` matrices along the first
dimension, we get :math:`X` as an array with dimensions
:math:`n \times k_t \times J`.

Correspondingly, :math:`\beta` is a vector with elements

.. math::

   (\beta_0^{\mu_1} \; \beta_0^{\mu_2} \; \beta_{x_1}^{\mu_1} \;
   \beta_{x_2}^{\mu_1} \; \beta_{x_3}^{\mu_2} \; \beta_{\textrm{land}})\prime

To multiply the :math:`X` array with dimensions
:math:`(n \times 6 \times 2)` and the :math:`(6 \times 1)` :math:`\beta`
vector, we *vectorize* over equations as follows:

.. sourcecode:: r

    eta <- apply(X, 3, '%*%', beta) 

The linear predictor eta is therefore a :math:`(n \times 2)` matrix.

The Memory-Efficient Layout
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Choosing a “compact” :math:`X` matrix and matrix :math:`\beta` is
probably the most memory-efficient configuration: model.matrix(…, shape
= “compact”) (the default) produces an :math:`n \times v` matrix, where
:math:`v` is the number of unique variables (5 in this case) [1]_ in all
of the equations. Let :math:`x_1` be an :math:`n \times 1` vector
representing variable x1, :math:`x_2` x2, and so forth.

.. math::

   \begin{aligned}
   X = (1 \; x_1 \; x_2 \; x_3 \; x_4) & & \beta = \left( \begin{array}{cc}

          \beta_0^{\mu_1}       & \beta_0^{\mu_2} \\
   \beta_{x_1}^{\mu_1}       & 0 \\
   \beta_{x_2}^{\mu_1}       & 0 \\
   \beta_{\textrm{land}} & \beta_{x_3}^{\mu_2} \\
   0                     & \beta_{\textrm{land}}
   \end{array} \right) \end{aligned}

The :math:`\beta_{\textrm{land}}` parameter is used twice to implement
the constraint, and the number of empty cells is minimized by
implementing the constraints in :math:`\beta` rather than :math:`X`.
Furthermore, since :math:`X` is :math:`(n \times 5)` and :math:`\beta`
is :math:`(5 \times 2)`, :math:`X\beta = \eta` is :math:`n
\times 2`.

Interchanging the Three Methods
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Continuing the bivariate probit example above, we only need to modify a
few lines of code to put these different schemes into effect. Using the
default (memory-efficient) options, the log-likelihood is:

.. sourcecode:: r

    bivariate.probit <- function(formula, data, start.val = NULL, ...) {
      fml <- parse.formula(formula, model = "bivariate.probit")
      D <- model.frame(fml, data = data)
      X <- model.matrix(fml, data = D, eqn = c("mu1", "mu2"))       # [1]
      Xrho <- model.matrix(fml, data = D, eqn = "rho")
      Y <- model.response(D)
      terms <- attr(D, "terms")
      start.val <- set.start(start.val, terms)
      start.val <- put.start(start.val, 1, terms, eqn = "rho")

      log.lik <- function(par, X, Y, terms) {
        Beta <- parse.par(par, terms, eqn = c("mu1", "mu2"))         # [2]
        gamma <- parse.par(par, terms, eqn = "rho")
        rho <- (exp(Xrho %*% gamma) - 1) / (1 + exp(Xrho %*% gamma))
        mu <- X %*% Beta                                             # [3]
        llik <- 0
        for (i in 1:nrow(mu)){
          Sigma <- matrix(c(1, rho[i,], rho[i,], 1), 2, 2)
          if (Y[i,1]==1)
            if (Y[i,2]==1)
              llik <- llik + log(pmvnorm(lower = c(0, 0), upper = c(Inf, Inf), 
                                         mean = mu[i,], corr = Sigma))
            else
              llik <- llik + log(pmvnorm(lower = c(0, -Inf), upper = c(Inf, 0), 
                                         mean = mu[i,], corr = Sigma))
          else
            if (Y[i,2]==1)
              llik <- llik + log(pmvnorm(lower = c(-Inf, 0), upper = c(0, Inf),
                                         mean = mu[i,], corr = Sigma))
            else
              llik <- llik + log(pmvnorm(lower = c(-Inf, -Inf), upper = c(0, 0), 
                                         mean = mu[i,], corr = Sigma))
            }
        return(llik)
      }
      res <- optim(start.val, log.lik, method = "BFGS",
                   hessian = TRUE, control = list(fnscale = -1),
                   X = X, Y = Y, terms = terms, ...)
      fit <- model.end(res, D)
      class(fit) <- "bivariate.probit"
      fit
    }

If you find that the default (memory-efficient) method isn’t the best
way to run your model, you can use either the intuitive option or the
computationally-efficient option by changing just a few lines of code as
follows:

-  **Intuitive option** At Comment [1]:

   .. sourcecode:: r

       X <- model.matrix(fml, data = D, shape = "stacked", eqn = c("mu1", "mu2"))

   and at Comment [2],

   .. sourcecode:: r

       Beta <- parse.par(par, terms, shape = "vector", eqn = c("mu1", "mu2"))

   The line at Comment [3] remains the same as in the original version.

-  **Computationally-efficient option** Replace the line at Comment [1]
   with

   .. sourcecode:: r

       X <- model.matrix(fml, data = D, shape = "array", eqn = c("mu1", "mu2"))

   At Comment [2]:

   .. sourcecode:: r

       Beta <- parse.par(par, terms, shape = "vector", eqn = c("mu1", "mu2"))

   At Comment [3]:

   .. sourcecode:: r

       mu <- apply(X, 3, '%*%', Beta)

Even if your optimizer calls a C or FORTRAN routine, you can use
combinations of model.matrix() and parse.par() to set up the data
structures that you need to obtain the linear predictor (or your model’s
equivalent) before passing these data structures to your optimization
routine.

.. [1]
   Why 5? In addition to the intercept term (a variable which is the
   same in either equation, and so counts only as one variable), the
   *unique* variables are :math:`x_1`, :math:`x_2`, :math:`x_3`, and
   :math:`x_4`.
