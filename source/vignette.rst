.. _vignettes:

Model Reference and Vignettes
=============================

This section includes technical information on the models currently implemented in Zelig (5.0-1). This includes a reference with a list of supported models as well as individual model vignettes with detailed information on the model, quantities of interest and syntax.

------------

Reference
~~~~~~~~~

The following models are currently supported in Zelig 5.0-1:

- :ref:`Exponential Regression <zexp>`: ``zexp$new()``
- :ref:`Gamma Regression <zgamma>`: ``zgamma()``
- :ref:`Logistic Regression <zlogit>`: ``zlogit$new()``
- :ref:`Log Normal Regression <zlognorm>`: ``zlognorm$new()``
- :ref:`Least Squares Regression <zls>`: ``zls$new()``
- :ref:`Negative Binomial Regression <znegbin>`: ``zbinom$new()``
- :ref:`Normal Regression <znorm>`: ``znormal$new()``
- :ref:`Poisson Regression <zpoisson>`: ``zpoisson$new()``
- :ref:`Probit Regression <zprobit>`: ``zprobit$new()``
- :ref:`Rare Events Logistic Regression <zrelogit>`: ``zrelogit$new()``
- :ref:`Tobit Regression <ztobit>`: ``ztobit$new()``

------------

.. include:: zelig-exp.rst

------------

.. include:: zelig-gamma.rst

------------

.. include:: zelig-logit.rst

------------

.. include:: zelig-lognorm.rst

------------

.. include:: zelig-ls.rst

------------

.. include:: zelig-negbin.rst

------------

.. include:: zelig-normal.rst

------------

.. include:: zelig-poisson.rst

------------

.. include:: zelig-probit.rst

------------

.. include:: zelig-relogit.rst

------------

.. include:: zelig-tobit.rst

.. toctree::
   :hidden:

   zelig-exp.rst
   zelig-gamma.rst
   zelig-logit.rst
   zelig-lognorm.rst
   zelig-ls.rst
   zelig-negbin.rst
   zelig-normal.rst
   zelig-poisson.rst
   zelig-probit.rst
   zelig-relogit.rst
   zelig-tobit.rst
