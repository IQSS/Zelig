
## ----, eval = FALSE------------------------------------------------------
## install.packages("Zelig", type = "source", repos = "http://r.iq.harvard.edu/")


## ----, eval = FALSE------------------------------------------------------
## # This installs devtools package, if not already installed
## install.packages("devtools")
## # This loads devtools   	
## library(devtools)
## # This downloads Zelig 5.0.1 from the IQSS Github repo
## install_github('IQSS/Zelig')


## ----, eval = TRUE, echo = FALSE-----------------------------------------
suppressWarnings(suppressMessages(library(Zelig5)))


## ----, eval = FALSE------------------------------------------------------
## library(Zelig)


## ----, eval = TRUE-------------------------------------------------------
# load toy dataset (when you install R, example datasets are also installed)
data(cars)
# initialize Zelig5 least squares object                            
z5 <- zls$new()  
# estimate ls model                     
z5$zelig(dist ~ speed, data = cars)
# you can now get model summary estimates
z5


## ----, eval = TRUE-------------------------------------------------------
# simulate over a range of speed between 10 and 20 mph
z5$setrange(speed = 10:20)

# you can also set covariates at particular value using $setx()
z5$setx(speed = 30)


## ----, eval = TRUE-------------------------------------------------------
#run 10 simulations and estimate quantities of interest
z5$sim(num = 10)
# default is 1,000 simulations


## ----, eval = TRUE-------------------------------------------------------
z5$sim.out$range


## ----QIs, eval = TRUE, fig.cap = "QIs"-----------------------------------
z5$graph()


