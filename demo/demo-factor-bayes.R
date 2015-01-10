# Zelig 4 code:
library(Zelig4)
data(swiss)
names(swiss) <- c("Fert", "Agr", "Exam", "Educ", "Cath", "InfMort")
z.out <- Zelig4::zelig(cbind(Agr, Exam, Educ, Cath, InfMort) ~ NULL,
                       model = "factor.bayes", data = swiss, factors = 2, verbose = TRUE,
                       a0 = 1, b0 = 0.15, burnin = 500, mcmc = 5000)

summary(z.out)
x.out <- Zelig4::setx(z.out, math = 30)
set.seed(42)
s.out <- Zelig4::sim(z.out, x = x.out, num = 1000)
summary(s.out)


# Zelig 5 code:
z5 <- zfactorbayes$new()

# z5$zelig(cbind(Agr, Exam, Educ, Cath, InfMort) ~ NULL,
#          data = swiss, factors = 2, verbose = TRUE,
#          a0 = 1, b0 = 0.15, burnin = 500, mcmc = 5000)

z5$zelig(~ Agr + Exam + Educ + Cath + InfMort,
         data = swiss, factors = 2, verbose = FALSE,
         a0 = 1, b0 = 0.15, burnin = 500, mcmc = 5000)

# z5$zelig(~Agriculture+Examination+Education+Catholic
#          +Infant.Mortality, factors=2,
#          lambda.constraints=list(Examination=list(1,"+"),
#                                  Examination=list(2,"-"), Education=c(2,0),
#                                  Infant.Mortality=c(1,0)),
#          verbose=0, store.scores=FALSE, a0=1, b0=0.15,
#          data=swiss, burnin=500, mcmc=500, thin=20)
z5

z.out <- zelig(~ Agr + Exam + Educ + Cath + InfMort,
               model = "factor.bayes", data = swiss,
               factors = 2, verbose = FALSE,
               a0 = 1, b0 = 0.15, burnin = 500, mcmc = 5000)

z.out <- zelig(~ Agr + Exam + Educ + Cath + InfMort,  
               model = "factor.bayes", data = swiss, factors = 2,
               lambda.constraints = list(Exam = list(1,"+"),
                                         Exam = list(2,"-"), Educ = c(2, 0),
                                         InfMort = c(1, 0)), 
               verbose = FALSE, a0 = 1, b0 = 0.15, 
               burnin = 500, mcmc = 5000)


lapply(z5$zelig.out$z.out, geweke.diag)

z5$zelig.call
z5$model.call
z5$zelig.out$z.out[[1]]
z5$setx()
# z5$setx(math = 30)
set.seed(42)
# z5$sim(num = 1000)
# z5$summarize()
z5$cite()

geweke.diag(z5$zelig.out$z.out[[1]])
heidel.diag(z5$zelig.out$z.out[[1]])
raftery.diag(z5$zelig.out$z.out[[1]])
summary(z5)
