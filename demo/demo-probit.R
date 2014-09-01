mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
mydata$rank <- factor(mydata$rank)

# Zelig 4 code:
library(Zelig4)
# epsilon does not seem to be taken into account
z.out <- zelig(admit ~ gre + gpa, data = mydata, model="probit",
               epsilon=.0001)
summary(z.out)
x.out <- setx(z.out, gpa=1.3, gre=4500, speed=12)
set.seed(42)
s.out <- sim(z.out, x = x.out, num=1000)
summary(s.out)

# Zelig 5 code:
z5 <- zprobit$new()
z5$zelig(admit ~ gre + gpa, data = mydata,
         epsilon=.0001)
# removed epsilon to compare with Zelig 4
z5$zelig(admit ~ gre + gpa, data = mydata)
z5
z5$setx(gpa=1.3, gre=4500, speed=12)
set.seed(42)
z5$sim(num=1000)
z5$summarize()
z5$cite()

