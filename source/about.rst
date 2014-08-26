.. _about:

About Zelig
-----------
Zelig is an open-source project developed and maintained by the `Data Science group <http://datascience.iq.harvard.edu/>`_ at Harvard's `Institute for Quantitative Social Science <http://iq.harvard.edu>`_. It was originally conceived and created by Kosuke Imai, Gary King, and Olivia Lau in 2007. The name is borrowed from Woody Allen's movie with the same name, Zelig. Leonard Zelig is a fictional character who takes on the characteristics of any strong personality around. Likewise, the Zelig statistical software easily adapts to any statistical model written in R, and in essence, takes the characteristics of any model.

Zelig leverages (R) code from many researchers and is designed to allow anyone to contribute their methods to it. Hence, we often refer to Zelig as "everyone's statistical software" and our aim is to make it, as well as the models it wraps, as accessible as possible. As such, it comes with self-contained documentation that minimizes startup costs, automates model summaries and graphics, and bridges existing R implementations through an intelligible call structure.

**License:** GPL-2 | GPL-3 [expanded from: GPL (>= 2)]

**Contact:** For questions, please join the Zelig mailing list:
`https://groups.google.com/forum/#!forum/zelig-statistical-software <https://groups.google.com/forum/#!forum/zelig-statistical-software>`_

`The Zelig Team <http://datascience.iq.harvard.edu/team>`_:

- Christine Choirat *(Lead Author)*
- James Honaker *(Project Lead)*
- Muhammed Y. Idris

*Original Authors:*

- `Gary King <http://gking.harvard.edu/>`_ *(Principle Investigator)*
- Kosuke Imai
- Olivia Lau

------------

Technical Vision
~~~~~~~~~~~~~~~~
Zelig is a framework for interfacing a wide range of statistical models and analytic methods in a common and simple way. Above and beyond estimation, Zelig adds considerable infrastructure to existing heterogeneous R implementations by translating hard-to-interpret coefficients into quantities of interest (e.g., expected and predicted values) through a simple call structure. This includes many specific methods, based on likelihood, frequentist, Bayesian, robust Bayesian and nonparametric theories of inference. Developers are encouraged to add their R packages to the Zelig toolkit by writing a few simple bridge functions.

Additional features include:

- Dealing with missing data by combining multiply imputed datasets
- Automating statistical bootstrapping
- Improving parametric procedures by leveraging nonparametric matching methods
- Evaluating counterfactuals
- Allowing conditional population and super population inferences
- Automating the creation of replication data files


Release Notes
~~~~~~~~~~~~~

**v 5.0-1**

This release provides a set of core models, while simplifying the model wrapping process, and solving architectural problems by completely rewriting into R’s Reference Classes for a fully object-oriented architecture.



