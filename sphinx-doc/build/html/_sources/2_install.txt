Installation
============

To use Zelig, you must install the statistical program R (if it is not
already installed), the Zelig package, and some R libraries (coda,
MCMCpack, sandwich, VGAM, and zoo).

Note: In this document, > denotes the R prompt.

If You Know R
^^^^^^^^^^^^^

We recommend that you launch R and type

.. sourcecode:: r

    > source("http://r.iq.harvard.edu/install.R")
    > library(Zelig)

then proceed to . For Windows R, you may edit the Rprofile file to load
Zelig automatically at launch (after which you will no longer need to
type library(Zelig) at startup). Simply add the line:

.. sourcecode:: r

    options(defaultPackages = c(getOption("defaultPackages"), "Zelig"))

If You Are New to R
^^^^^^^^^^^^^^^^^^^

If you are new to R, we recommend that you read the following section on
installation procedures as well as the overview of R syntax and usage in
.

This distribution works on a variety of platforms, including Windows
(see ), MacOSX (see ), and Linux (see ). Alternatively, you may access R
from your PC using a terminal window or an X-windows tunnel to a Linux
or Unix server (see ). Most servers have R installed; if not, contact
your network administrator.

There are advantages and disadvantages to each type of installation. On
a personal computer, R is easier to install and launch. Using R remotely
on a server requires a bit more set-up, but does not tie up your local
CPU, and allows you to take advantage of the server’s speed.

Windows
-------

Installing R
^^^^^^^^^^^^

Go to the Comprehensive R Archive Network website
`(http://www.r-project.org) <(http://www.r-project.org)>`__ and download
the latest at Double-click the .exe file to launch the R installer. We
recommend that you accept the default installation options if this your
first installation.

Installing Zelig
^^^^^^^^^^^^^^^^

Once R is installed, you must install the Zelig and VGAM packages. There
are three ways to do this.

#. We recommend that you start R and then type:

   .. sourcecode:: r

       > source("http://r.iq.harvard.edu/zelig/install.R")
       > library(Zelig)

#. Alternatively, you may install each component package individually in
   R:

   .. sourcecode:: r

       > install.packages("Zelig")
       > install.packages("zoo")
       > install.packages("sandwich")
       > install.packages("MCMCpack")
       > install.packages("coda")
       > install.packages("lattice")
       > install.packages("mvtnorm")
       > install.packages("VGAM")
       > install.packages("sna")
       > install.packages("systemfit")
       > install.packages("nnet")
       > install.packages("gee")
       > install.packages("mgcv")
       > library(Zelig)

   Zelig will load the optional libraries whenever their functions are
   needed; it is not necessary to load any package other than Zelig at
   startup.

#. [manual.windows]Alternatively, you may use the drop down menus to
   install Zelig. This requires four steps.

   #. [win.zelig] Go to and and download the latest release of Zelig.
      The VGAM, MCMCpack, coda, zoo, and sandwich packages are available
      from . Store these ``.zip`` files in your R program directory. For
      example, the default R program directory is
      C:\ :math:`\backslash`\ Program
      Files\ :math:`\backslash`\ R\ :math:`\backslash`\ :math:`\backslash`. [1]_

   #. Start R. From the drop-down menus, select the “Packages” menu and
      then the “Install Files from Local Zip Files” option.

   #. A window will pop up, allowing you to select one of the downloaded
      files for installation. There is no need to unzip the files prior
      to installation. Repeat and select the other downloaded file for
      installation.

   #. At the R prompt, type ``library(Zelig)`` to load the functionality
      described in this manual. Note that Zelig will automatically load
      the other libraries as necessary.

#. An additional *recommended but optional step* is to set up R to load
   Zelig automatically at launch. (If you skip this step, you must type
   library(Zelig) at the beginning of every R session.) To automate this
   process, edit the Rprofile file located in the R program subdirectory
   (C:\ :math:`\backslash`\ Program
   Files\ :math:`\backslash`\ R\ :math:`\backslash`\ :math:`\backslash`\ etc\ :math:`\backslash`
   in our example). Using a text editor such as Windows notepad, add the
   following line to the Rprofile file:

   .. sourcecode:: r

       options(defaultPackages = c(getOption("defaultPackages"), "Zelig"))

Zelig is distributed under the . After installation, the source code is
located in your R library directory, which is by default
C:\ :math:`\backslash`\ Program
Files\ :math:`\backslash`\ R\ :math:`\backslash`\ :math:`\backslash`\ library\ :math:`\backslash`\ Zelig\ :math:`\backslash`.

Updating Zelig
^^^^^^^^^^^^^^

There are two ways to update Zelig.

#. We recommend that you periodically update Zelig at the R prompt by
   typing:

   .. sourcecode:: r

       > update.packages()
       > library(Zelig)

#. Alternatively, you may use the procedure outlined in to periodically
   update Zelig. Simply download the latest .zip file and follow the
   four steps.

MacOS X
-------

Installing R
^^^^^^^^^^^^

If you are using MacOS X, you may install the latest version of R ( at
this time) from the CRAN website . At this time, Zelig is not supported
for R on MacOS 8.6 through 9.x.

Installing Zelig
^^^^^^^^^^^^^^^^

Once R is installed, you must install the Zelig and VGAM packages. There
are several ways to do this.

#. **For RAqua**:

   #. We recommend that you start R, and then type:

      .. sourcecode:: r

          > source("http://r.iq.harvard.edu/zelig/install.R")
          > library(Zelig)

      (You may ignore the warning messages, unless they say “Non-zero
      exit status”.)

   #. Alternatively, to avoid the warning messages, you need to install
      each package individually and specify the specific installation
      path:

      .. sourcecode:: r

          > install.packages("Zelig", lib = "~/Library/R/library")
          > install.packages("zoo", lib = "~/Library/R/library")
          > install.packages("sandwich", lib = "~/Library/R/library")
          > install.packages("MCMCpack", lib = "~/Library/R/library")
          > install.packages("coda", lib = "~/Library/R/library")
          > install.packages("lattice", lib = "~/Library/R/library")
          > install.packages("mvtnorm", lib = "~/Library/R/library")
          > install.packages("VGAM", lib = "~/Library/R/library")
          > install.packages("sna", lib = "~/Library/R/library")
          > install.packages("systemfit", lib = "~/Library/R/library")
          > install.packages("nnet", lib = "~/Library/R/library")
          > install.packages("gee", lib = "~/Library/R/library")
          > install.packages("mgcv", lib = "~/Library/R/library")
          > library(Zelig)

      where ``~/Library/R/library`` is the default local library
      directory. Zelig will load the other libraries whenever their
      functions are needed; it is not necessary to load these packages
      at startup.

   #. Alternatively, you may use the drop down menus to install Zelig.
      This requires three steps.

      #. Go to and download the latest release of Zelig. The VGAM,
         MCMCpack, coda, zoo, and sandwich packages are available from .
         Save these ``.tar.gz`` files in a convenient place.

      #. Start R. From the drop-down menus, select the “Packages” menu
         and then the “Install Files from Local Files” option.

      #. A window will pop up, allowing you to select the one of the
         downloaded files for installation. There is no need to unzip
         the files prior to installation. Repeat and select the other
         downloaded file for installation.

#. **For command line R:**

   #. Before installing command line R, you need to create a local R
      library directory. If you have done so already, you may skip to
      the next step. Otherwise, at the terminal prompt in your home
      directory, type:

      .. sourcecode:: r

          % mkdir ~/Library/R ~/Library/R/library

   #. Modify your configuration file to identify ~/Library/R/library as
      your R library directory. There are two ways of doing this:

      #. Open the .Renviron file (or create one, if you don’t have one)
         and add the following line:

         .. sourcecode:: r

             R_LIBS = "~/Library/R/library"

      #. *Alternatively*, you may modify your shell configuration file.
         For a Bash shell, open your .bashrc file and add the following
         line:

         .. sourcecode:: r

             export R_LIBS="$HOME/Library/R/library"

   #. Start R and at the prompt, type:

      .. sourcecode:: r

          > source("http://r.iq.harvard.edu/zelig/install.R")
          > library(Zelig)

      (You may ignore the warning messages, unless they say “Non-zero
      exit status”.)

   #. Alternatively, to avoid the warning messages, you need to install
      each component package separately and specify the installation
      path:

      .. sourcecode:: r

          > install.packages("Zelig", lib = "~/Library/R/library")
          > install.packages("zoo", lib = "~/Library/R/library")
          > install.packages("sandwich", lib = "~/Library/R/library")
          > install.packages("MCMCpack", lib = "~/Library/R/library")
          > install.packages("coda", lib = "~/Library/R/library")
          > install.packages("lattice", lib = "~/Library/R/library")
          > install.packages("mvtnorm", lib = "~/Library/R/library")
          > install.packages("VGAM", lib = "~/Library/R/library")
          > install.packages("sna", lib = "~/Library/R/library")
          > install.packages("systemfit", lib = "~/Library/R/library")
          > install.packages("nnet", lib = "~/Library/R/library")
          > install.packages("gee", lib = "~/Library/R/library")
          > install.packages("mgcv", lib = "~/Library/R/library")
          > library(Zelig)

      Although the lib argument is optional, we recommend that you set
      it to the default RAqua directory (``"~/Library/R/library"``), in
      case you later decide to install the RAqua GUI (which has a
      different default directory).

At the R prompt, type ``library(Zelig)`` to load the functionality
described in this manual. Note that Zelig will automatically load the
other packages as necessary.

Zelig is distributed under the . After installation, the source code is
located in your R library directory, ~/Library/R/library/Zelig/.

Updating Zelig
^^^^^^^^^^^^^^

There are two ways to update Zelig.

#. We recommend that you start R and, at the R prompt, type:

   .. sourcecode:: r

       > update.packages()

#. Alternatively, you may remove an old version by command by typing R
   CMD REMOVE Zelig at the terminal prompt. Then download and reinstall
   the package using the installation procedures  outlined above.

UNIX and Linux
--------------

Installing R
^^^^^^^^^^^^

Type R at the terminal prompt (which we denote as % in this section) to
see if R is available. (Typing ``q()`` will enable you to quit.) If it
is installed, proceed to the next section. If it is not installed and
you are not the administrator, contact that individual, kindly request
that they install R on the server, and continue to the next section. If
you have administrator privileges, you may download the latest release
at the website. Although installation varies according to your Linux
distribution, we provide an example for Red Hat Linux 9.0 as a guide:

#. Log in as root.

#. Download the appropriate binary file for Red Hat 9 from CRAN. For
   example, for Red Hat 9 running on the Intel 386 platform, go to
   http://cran.r-project.org/bin/linux/.

#. Type the following command at the terminal prompt:

Installing Zelig
^^^^^^^^^^^^^^^^

Before installing Zelig, you need to create a local R library directory.
If you have done so already, you can skip to . If not, you must do so
before proceeding because most users do not have authorization to
install programs globally. Suppose we want the directory to be
~/.R/library. At the terminal prompt in your home directory, type:

.. sourcecode:: r

    % mkdir ~/.R ~/.R/library

Now you are ready to install Zelig.[unix.zelig] There are two ways to
proceed.

#. Recommended procedure:

   #. Open the ~/.Renviron file (or create it if it does not exist) and
      add the following line:

      .. sourcecode:: r

          R_LIBS = "~/.R/library"

      You only need to perform this step once.

   #. Start R. At the R prompt, type:

      .. sourcecode:: r

          > source("http://r.iq.harvard.edu/zelig/install.R")
          > library(Zelig)

      (You may ignore the warning messages, unless they say “Non-zero
      exit status”.)

   #. Alternatively, you can avoid the warning messages by installing
      each component package separately and specifying the installation
      path:

      .. sourcecode:: r

          > install.packages("Zelig", lib = "~/Library/R/library")
          > install.packages("zoo", lib = "~/Library/R/library")
          > install.packages("sandwich", lib = "~/Library/R/library")
          > install.packages("MCMCpack", lib = "~/Library/R/library")
          > install.packages("coda", lib = "~/Library/R/library")
          > install.packages("lattice", lib = "~/Library/R/library")
          > install.packages("mvtnorm", lib = "~/Library/R/library")
          > install.packages("VGAM", lib = "~/Library/R/library")
          > install.packages("sna", lib = "~/Library/R/library")
          > install.packages("systemfit", lib = "~/Library/R/library")
          > install.packages("nnet", lib = "~/Library/R/library")
          > install.packages("gee", lib = "~/Library/R/library")
          > install.packages("mgcv", lib = "~/Library/R/library")
          > library(Zelig)

   #. Finally, create a .Rprofile file in your home directory,
      containing the line:

      .. sourcecode:: r

          library(Zelig)

      This will load Zelig every time you start R.

#. [unix.manual] Alternatively:

   #. Add the local R library directory that you created above
      (~/.R/library in the example) to the environmental variable
      R\_LIBS.

   #. Download the latest bundles for Unix from the , and (for the VGAM,
      MCMCpack, coda, sandwich, and zoo packages) from the website.

   #. If XX is the current version number, at the terminal prompt, type:

      .. sourcecode:: r

          % R CMD INSTALL Zelig_XX.tar.gz
          % R CMD INSTALL zoo_XX.tar.gz
          % R CMD INSTALL sandwich_XX.tar.gz
          % R CMD INSTALL MCMCpack_XX.tar.gz
          % R CMD INSTALL coda_XX.tar.gz
          % R CMD INSTALL lattice_XX.tar.gz
          % R CMD INSTALL mvtnorm_XX.tar.gz
          % R CMD INSTALL VGAM_XX.tar.gz
          % R CMD INSTALL sna_XX.tar.gz
          % R CMD INSTALL systemfit_XX.tar.gz
          % R CMD INSTALL nnet_XX.tar.gz
          % R CMD INSTALL gee_XX.tar.gz
          % R CMD INSTALL mgcv_XX.tar.gz

          % rm Zelig_XX.tar.gz zoo_XX.tar.gz sandwich_XX.tar.gz MCMCpack_XX.tar.gz coda_XX.tar.gz lattice_XX.tar.gz mvtnorm_XX.tar.gz mvtnorm_XX.tar.gz VGAM_XX.tar.gz sna_XX.tar.gz systemfit_XX.tar.gz nnet_XX.tar.gz  gee_XX.tar.gz mgcv_XX.tar.gz

   #. Create a .Rprofile file in your home directory, containing the
      line:

      .. sourcecode:: r

          library(Zelig)

      This will load Zelig every time you start R.

Zelig is distributed under the . After installation, the source code is
located in your R library directory. If you followed the example above,
this is  /.R/library/Zelig/.

Updating Zelig
^^^^^^^^^^^^^^

There are two ways to update Zelig.

#. We recommend that you start R and, at the R prompt, type:

   .. sourcecode:: r

       > update.packages()

#. Alternatively, you may remove an old version by command by typing R
   CMD REMOVE Zelig at the terminal prompt. Then download and reinstall
   the package using the installation procedure Section
   [sss:unix.library] outlined above.

Version Compatability
---------------------

In addition to R itself, Zelig also depends on several R packages
maintained by other development teams. Although we make every effort to
keep the latest version of Zelig up-to-date with the latest version of
those packages, there may occasionally be incompatabilities. See
[table.compat] in the Appendix for a list of packages tested to be
compatabile with a given Zelig release. You may obtain older versions of
most packages at http://www.r-project.org.

.. [1]
   Note that when updating R to the latest release, the installer does
   not delete previous versions from your
   C:\ :math:`\backslash`\ Program
   Files\ :math:`\backslash`\ R\ :math:`\backslash` directory. In this
   example, the subdirectory :math:`\backslash`\ :math:`\backslash`
   stores R version . Thus, if you have a different version of R
   installed, you should change the last part of the R program directory
   file path accordingly.
