library(MASS)
mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
mydata$rank <- factor(mydata$rank)

# Zelig 4 code:
library(Zelig4)
z.out <- Zelig4::zelig(admit ~ gre + gpa, data = mydata,
                       model="probit.bayes")
summary(z.out)
x.out <- Zelig4::setx(z.out, gpa = 1.3, gre = 4500, speed = 12)
set.seed(42)
s.out <- Zelig4::sim(z.out, x = x.out, num = 1000)
summary(s.out)

# Zelig 5 code:
z5 <- zprobitbayes$new()
z5$zelig(admit ~ gre + gpa, data = mydata)
z5
z5$zelig.out
z5$setx(gpa = 1.3, gre = 4500, speed = 12)
set.seed(42)
z5$sim(num = 1000)
z5$summarize()
z5$cite()

z.out <- zelig(admit ~ gre + gpa, data = mydata, model = "probit.bayes")
z.out
