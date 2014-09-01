library(MASS)
mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
mydata$rank <- factor(mydata$rank)

# Zelig 4 code:
library(Zelig4)
# epsilon does not seem to be taken into account
z.out <- zelig(admit ~ gre + gpa, data = mydata, model="logit",
               epsilon=.0001)
summary(z.out)
x.out <- setx(z.out, gpa=1.3, gre=4500, speed=12)
set.seed(42)
s.out <- sim(z.out, x = x.out, num=1000)
summary(s.out)


# Zelig 5 code:
z5 <- zlogit$new()
z5$zelig(admit ~ gre + gpa, data = mydata,
         epsilon=1)
z5
z5$zelig.out
# removed epsilon to compare with Zelig 4
z5$zelig(admit ~ gre + gpa, data = mydata)
z5
z5$setx(gpa=1.3, gre=4500, speed=12)
set.seed(42)
z5$sim(num=1000)
z5$summarize()
z5$cite()

# Example 2
data(turnout)
z.out <- zelig(vote ~ race + educate,
               data = turnout,
               model = "logit")
summary(z.out)
x.out <- setx(z.out, educate = 12)
set.seed(42)
s.out <- sim(z.out, x = x.out, num = 1000)
summary(s.out)

z5 <- zlogit$new()
z5$zelig(vote ~ race + educate,
         data = turnout)
z5$show()
z5$setx(educate = 12)
set.seed(42)
z5$sim(num = 1000)
z5$summarize()

