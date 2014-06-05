
## ----, eval=FALSE--------------------------------------------------------
## # Run least squares regression and save the output in working memory:
## z.out <- zelig(y ~ x1 + x2, model = "ls", data = mydata)
## # See what's in the R object:
## names(z.out)
## ## [1] "coefficients" "residuals" "effects" "rank"
## # Extract and display the coefficients in z.out:
## z.out$coefficients


## ----, eval=FALSE--------------------------------------------------------
## library(foreign) # Load the foreign package.
## stata.data <- read.dta("mydata.dta") # For Stata data.
## spss.data <- read.spss("mydata.sav", to.data.frame = TRUE) # For SPSS.


## ----, eval=FALSE--------------------------------------------------------
## library(Zelig) # Loads the Zelig library.
## data(turnout) # Loads the turnout data.


## ----, eval=FALSE--------------------------------------------------------
## read.csv("mydata.csv", header = TRUE)


## ----, eval=FALSE--------------------------------------------------------
## read.table("mydata.tab", header = TRUE, na.strings = "-9")


## ----, eval=FALSE--------------------------------------------------------
## data <- read.csv("mydata.csv", header = TRUE) # Read the data.
## dim(data) # Displays the dimensions of the data frame
## ## [1] 16000 8 # in rows then columns.
## data[1:10, ] # Display rows 1-10 and all columns.
## names(data) # Check the variable names.
## ## [1] "V1" "V2" "V3"
## # These values indicate that the variables weren't named, and took default values.
## names(data) <- c("income", "educate", "year") # Assign variable names.
## summary(data) # Returning a summary for each variable.


## ----, eval=FALSE--------------------------------------------------------
## ## Saves `mydata' to `mydata.RData' in your working directory.
## save(mydata, file = "mydata.RData")
## ## Saves your entire workspace to the default `.RData' file.
## save.image()


## ----, eval=FALSE--------------------------------------------------------
## load("mydata.RData")


## ----, eval=FALSE--------------------------------------------------------
## ## Creates 'logic' (5 T/F values).
## logic <- c(TRUE, FALSE, TRUE, TRUE, TRUE)
## ## All integers between 10 and 20.
## var1 <- 10:20
## ## Sequence from 5 to 10 by intervals of 0.5.
## var2 <- seq(from = 5, to = 10, by = 0.5)
## ## 20 'NA' values.
## var3 <- rep(NA, length = 20)
## ## 15 '1's followed by 15 '0's.
## var4 <- c(rep(1, 15), rep(0, 15))


## ----, eval=FALSE--------------------------------------------------------
## var3 <- log(var2) - 2 * var1 # Create 'var3' using math operations.


## ----, eval=FALSE--------------------------------------------------------
## var3 <- var1 < var2
## var3 <- var1 == var2


## ----, eval=FALSE--------------------------------------------------------
## ## Copies 'var1' from 'data', creating 'var'.
## var <- data$var1


## ----, eval=FALSE--------------------------------------------------------
## ## Replace 'var1' in `data' with 'var'.
## data$var1 <- var
## ## Generate 'new.var' in 'data' using 'var'.
## data$new.var <- var


## ----, eval=FALSE--------------------------------------------------------
## data$var1 <- NULL


## ----, eval=FALSE--------------------------------------------------------
## var3 <- var1 + 2 * var2
## var3 <- log(var1)


## ----, eval=FALSE--------------------------------------------------------
## new.data <- cbind(data$dep.var, data$var1, data$var2, data$var3)


## ----, eval=FALSE--------------------------------------------------------
## new.data <- na.omit(new.data)


