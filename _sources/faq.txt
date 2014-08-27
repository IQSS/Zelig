.. _faq:

Frequently Asked Questions
==========================

If you find a bug, or cannot figure something out after reading through the FAQs below, please send your question to the Zelig listserv at: `https://groups.google.com/forum/#!forum/zelig-statistical-software <https://groups.google.com/forum/#!forum/zelig-statistical-software>`_. Please explain exactly what you did and include the full error message, including the traceback(). You should get an answer from the developers or another user in short order.

--------

Why can’t I install Zelig?
~~~~~~~~~~~~~~~~~~~~~~~~~~
We recommend that you first check your internet connection, as you must be connected to install packages. In addition, there are a few platform-specific reasons why you may be having installation problems:

-  **On Windows**: If you are using the very latest version of R, you may not be able to install Zelig until we update Zelig to work with this latest release. Currently Zelig 5.0-1 is compatible with R (>= 3.0.2). If you wish to install Zelig in the interim, install the appropriate version of R and try to reinstall Zelig.

-  **On Mac or Linux systems**: If you get the following warning message at the end of your installation:

   .. sourcecode:: r

       > Installation of package VGAM had non-zero exit status in ...

   this means that you were not able to install VGAM properly. Make sure that you have the g77 Fortran compiler. For Intel Macs, download the Apple developer tools. After installation, try to install Zelig again.

If neither solution works, feel free email the Zelig mailing list directly at: `https://groups.google.com/forum/#!forum/zelig-statistical-software <https://groups.google.com/forum/#!forum/zelig-statistical-software>`_.

--------

Why can’t I install R?
~~~~~~~~~~~~~~~~~~~~~~
If you have problems installing R, you should search the internet for the R help mailing list, check out technical Q & A forums (e.g., StackOverflow), or email the Zelig mailing list directly at: `https://groups.google.com/forum/#!forum/zelig-statistical-software <https://groups.google.com/forum/#!forum/zelig-statistical-software>`_.

--------

Why can’t I load data?
~~~~~~~~~~~~~~~~~~~~~~
It is likely that the reason you are unable to load data because you have not specified the correct working directory (e.g., the location of the data you are trying to load). You should specify you working directory use the ``setwd()`` function in which you will include the the file path to your working director. For example, if I wanted to load a file that is my *Documents* folder, I must first:

.. sourcecode:: r

    > setwd("path/to/Documents")

File paths can be found by right clicking the working directory folder in any file browser and clicking "Get Info" (on Mac) or "Properties" (on Windows). Black-slashes (\\) in file paths copied from the "Properties" link on Windows machines must be replace with forward-slashes (/). For example, the Windows path: ``C:\Program Files\R``, would be typed as ``C:/Program Files/R``.

--------

R is neat. How can I find out more?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
R is a collective project with contributors from all over the world. Their website (`http://www.r-project.org <http://www.r-project.org>`_) has more information on the R project, R packages, conferences, and other learning material.

