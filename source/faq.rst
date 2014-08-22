.. _faq:

Frequently Asked Questions
==========================

If you find a bug, or cannot figure something out after reading through the FAQs below, please send your question to the Zelig listserv at: `https://groups.google.com/forum/#!forum/zelig-statistical-software <https://groups.google.com/forum/#!forum/zelig-statistical-software>`_. Please explain exactly what you did and include the full error message, including the traceback(). You should get an answer from the developers or another user in short order.

--------

How do I cite Zelig?
~~~~~~~~~~~~~~~~~~~~
We would appreciate if you would cite Zelig as:

  Imai, Kosuke, Gary King and Olivia Lau. 2006. “Zelig: Everyone’s Statistical Software,” http://GKing.Harvard.Edu/zelig.

Please also cite the contributors for the models or methods you are using. These citations can be found in each individual model's vignette which can be found in the the :ref:`vignettes:`.

--------

Why can’t I install Zelig?
~~~~~~~~~~~~~~~~~~~~~~~~~~
We recommend that you first check your internet connection, as you must be connected to install packages. In addition, there are a few platform-specific reasons why you may be having installation problems:

-  **On Windows**: If you are using the very latest version of R, you may not be able to install Zelig until we update Zelig to work with this latest release. Currently Zelig 5.0.1 is compatible with R(≥ 3.0.2). If you wish to install Zelig in the interim, install the appropriate version of R and try to reinstall Zelig.

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

How do I increase the memory for R?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Windows users may get the error that R has run out of memory. If you've installed more memory on your machine, you may have to reinstall R in order to take advantage of the additional capacity.

You may also set the amount of available memory manually. Close R, then right-click on your R program icon (the icon on your desktop or in your programs directory). Select “Properties”, and then select the “Shortcut” tab. Look for the “Target” field and after the closing quotes around the location of the R executable, add

.. sourcecode:: r

    --max-mem-size=500M

You may increase this value up to 2GB or the maximum amount of physical RAM you have installed. If you get the error that R cannot allocate a vector of length x, close out of R and add the following line to the “Target” field:

.. sourcecode:: r

    --max-vsize=500M

or as appropriate.

You can always check to see how much memory R has available by typing at the R prompt

.. sourcecode:: r

    > round(memory.limit()/2^20, 2)

which gives you the amount of available memory in MB.

--------

Why doesn’t the pdf print properly?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Zelig uses several special LaTeX environments. If the pdf looks right on the screen, there are two possible reasons why it’s not printing properly:

-  Adobe Acrobat isn’t cleaning up the document. Updating to Acrobat
   Reader 6.0.1 or higher should solve this problem.

-  Your printer doesn’t support PostScript Type 3 fonts. Updating your
   print driver should take care of this problem.

--------

R is neat. How can I find out more?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
R is a collective project with contributors from all over the world. Their website (`r-project.org <https://r-project.org>`_.) has more information on the R project, R packages, conferences, and other learning material.

