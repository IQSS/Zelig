
## ----, eval = FALSE------------------------------------------------------
## install.packages("Zelig", type = "source", repos = "http://r.iq.harvard.edu/")


## ----, eval = FALSE------------------------------------------------------
## # This installs devtools package, if not already installed
## install.packages("devtools")
## # This loads devtools   	
## library(devtools)
## # This downloads Zelig 5.0-1 from the IQSS Github repo
## install_github('IQSS/Zelig')


## ----, eval = TRUE, echo = FALSE-----------------------------------------
suppressWarnings(suppressMessages(library(Zelig)))


## ----, eval = FALSE------------------------------------------------------
## library(Zelig)


## ----Scatterplot, eval = TRUE, fig.cap = "Scatterplot"-------------------
# Scatterplot of car speed and distance required for full stop	
plot(cars$speed, cars$dist, main = "Scatterplot of car speed and distance required for full stop", ylab = "Distance (feet)", xlab = "Speed (miles per hour)")
# Fit regression line to data 
abline(lm(cars$dist ~ cars$speed), col = "firebrick")


## ----, eval = TRUE-------------------------------------------------------
# load dataset (when you install R, example datasets are also installed)
data(cars)
# initialize Zelig5 least squares object                            
z5 <- zls$new()
# estimate ls model                     
z5$zelig(dist ~ speed, data = cars)
# you can now get model summary estimates
summary(z5)


## ----, eval = TRUE-------------------------------------------------------
# set speed to 30
z5$setx(speed = 30)

# set speed to 50
z5$setx1(speed = 50)


## ----, eval = TRUE-------------------------------------------------------
# run simulations and estimate quantities of interest
z5$sim()
z5


## ----, eval = TRUE-------------------------------------------------------
z5$sim.out


## ----QIs, eval = TRUE, fig.cap = "QIs"-----------------------------------
z5$graph()


## ----help, eval = FALSE--------------------------------------------------
## # documentation for least squares model
## z5 <- zls$new()
## z5$help()
## 
## # documentation for logistic regression
## z5 <- zlogit$new()
## z5$help()


