
Welcome to Zelig!
=================

**Zelig** is a framework with an easy-to-use program that can estimate, help interpret, and present the results of a large range of statistical methods. It literally is "everyone's statistical software" because Zelig uses (R) code from many researchers. We also hope it will become "everyone's statistical software" for applications, and we have designed it so that anyone can use it or add their methods to it. Zelig comes with detailed, self-contained documentation that minimizes startup costs for Zelig and R (with all methods described in exactly the same notation, syntax, and style), automates graphics and summaries for all models, and, with only three simple required commands, makes the power of R accessible for all users. Zelig also works well for teaching, and is designed so that scholars can use the same program with students that they use for their research. Zelig is built on a wide ranging `ontology of statistical methods <http://gking.harvard.edu/files/abs/z-abs.shtml>`_.


Zelig adds considerable infrastructure to improve the use of existing methods. It interfaces with a wide range of statistical models through a simple and common intelligible call structure. It generalizes the program Clarify (for Stata), which translates hard-to-interpret coefficients into quantities of interest; combines multiply imputed data sets (such as output from Amelia) to deal with missing data; automates bootstrapping for all models; uses sophisticated nonparametric matching commands which improve parametric procedures (via MatchIt); allows one-line commands to run analyses in all designated strata; automates the creation of replication data files so that you (or, if you wish, anyone else) can replicate the results of your analyses (hence satisfying the replication standard); makes it easy to evaluate counterfactuals (via WhatIf); and allows conditional population and superpopulation inferences. Zelig includes many specific methods, based on likelihood, frequentist, Bayesian, robust Bayesian, and nonparametric theories of inference.  Developers make their R packages usable from Zelig by writing a few simple bridge functions.


For users, see the :ref:`installation_quickstart` guide and then a full PDF of the documentation `here <http://zeligproject.org/build/latex/Zelig.pdf>`_. Please also join our `Zelig Google Group <https://groups.google.com/forum/#!forum/zelig-statistical-software>`_, where you can ask questions, report bugs, and help others.


For developers, to view the code-base, visit the source repository at `https://github.com/IQSS/Zelig <https://github.com/IQSS/Zelig>`_ and for regular updates  and release information be sure to follow us on twitter at `@IQSS <https://twitter.com/IQSS>`_. 

You can also find the 
 
.. toctree::
   :hidden:

   installation_quickstart.rst
   vignette.rst
   faq.rst
   about.rst

*Inheritance Tree*

.. raw:: html

    <div style="margin-top:10px;">
      <iframe width="835" height="550" src="http://bl.ocks.org/myi100/raw/1ca9ba696aa0bb462415/" marginwidth="0" marginheight="0" scrolling="yes" frameBorder="0"></iframe>
    </div>
