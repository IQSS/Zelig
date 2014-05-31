
##  Attaching the sample turnout dataset:
data(turnout)

##  Estimate the model:
z.out <- zelig(vote ~ race + educate + age + I(age^2) + income,
               model = "logit", data = turnout)
user.prompt()
summary(z.out)

##  Creating setx structures with education set to high school and
##  post-college levels, for the whole range of the age variable.  
user.prompt()
x.low <- setx(z.out, educate = 12, age = 18:95)
x.high <- setx(z.out, educate = 16, age = 18:95)

##  Using sim to generate the simulated predicted probabilites:
user.prompt()
s.out <- sim(z.out, x = x.low, x1 = x.high)
user.prompt()
plot.ci(s.out, xlab = "Age in Years",
        ylab = "Predicted Probability of Voting",
        main = "Effect of Education and Age on Voting Behavior")

text(x=50,y=.95,labels="College Education (16 years)",cex=0.6)
text(x=60,y=.8,labels="High School Education (12 years)",cex=0.6)














