.. _about:

About Zelig
-----------
Zelig is an open-source project developed and maintained by the `Data Science group <http://datascience.iq.harvard.edu/>`_ at Harvard's `Institute for Quantitative Social Science <http://iq.harvard.edu>`_. It was conceived and created by Kosuke Imai, Gary King, and Olivia Lau in 2007. It is named for Leonard Zelig, a fictional character in a Woody Allen movie who takes on the personality of anyone around him, and thus fits into any situation. Likewise, Zelig software easily adapts to any statistical model and similarly fits into any situation.

Zelig leverages (R) code from many researchers and is designed to allow anyone to contribute their methods to it. Hence, we often refer to Zelig as "everyone's statistical software" and our aim is to make it, as well as the models it wraps, as accessible as possible. As such, it comes with self-contained documentation that minimizes startup costs, automates model summaries and graphics, and bridges existing R implementations through an intelligible call structure.

**Contact:** For questions, please join the `Zelig mailing list <https://groups.google.com/forum/#!forum/zelig-statistical-software>`_.

**Principal Investigator:** `Gary King <http://gking.harvard.edu/>`_

**Project Team Leads:** James Honaker, Christine Choirat, Muhammed Y. Idris

**Original Authors:** Kosuke Imai, Gary King, Olivia Lau

**Contributors:**  Christine Choirat, Matt Owen, Justin Grimmer, Jason Wittenberg, Badri Narayan Bhaskar, Skyler J. Cranmer, Ben Goodrich, Ying Lu, Patrick Lam, Nicholas Carnes, Alexander D'Amour, Delia Bailey, Ferdinand Alimadhi, Elena Villalon

**To Cite Zelig,** please reference these two sources:

  Kosuke Imai, Gary King, and Olivia Lau. 2007. "Zelig: Everyone's Statistical Software,"  http://zeligproject.org.

  Imai, Kosuke, Gary King, and Olivia Lau. 2008. "Toward A Common Framework for Statistical Analysis and Development." Journal of Computational and Graphical Statistics, Vol. 17, No. 4 (December), pp. 892-913, http://j.mp/msE15c.

**License:** GPL-2 | GPL-3 [expanded from: GPL (>= 2)]

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



