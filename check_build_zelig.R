# Check and build Zelig

# Ensure that IQSSDevtools is installed
if(!('IQSSdevtools' %in% installed.packages()[, 1]))
    devtools::install_github('IQSS/IQSSdevtools')

# Check and build package and website
#-- Roxygen is only used for user documentation, not collating and building
#--   NAMESPACE
#-- Vignettes are used to construct the website, but do to CRAN file size
#--   constraints are not included with the package source build.
library(IQSSdevtools)
build_iqss_package(rdocs_args = "rd", build_args = "--no-build-vignettes")
