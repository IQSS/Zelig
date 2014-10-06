
## ----, eval = FALSE------------------------------------------------------
## z5 <- zfactorbayes$new()
## z5$zelig(cbind(Y1 ,Y2, Y3) ~ NULL, factors = 2,
##          model = "factor.bayes", data = mydata)


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(cbind(Y1 ,Y2, Y3) ~ NULL, factors = 2,
##                model = "factor.bayes", data = mydata)


## ----, eval = FALSE------------------------------------------------------
## data(swiss)
## names(swiss) <- c("Fert", "Agr", "Exam", "Educ", "Cath", "InfMort")


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(cbind(Agr, Exam, Educ, Cath, InfMort) ~ NULL,
##                model = "factor.bayes", data = swiss, factors = 2, verbose = TRUE,
##                a0 = 1, b0 = 0.15, burnin = 5000, mcmc = 50000)


## ----, eval = FALSE------------------------------------------------------
## algor <- try(geweke.diag(z.out$coefficients), silent=T)
## if (class(algor) == "try-error")
##     print(algor)


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(cbind(Agr, Exam, Educ, Cath, InfMort) ~ NULL,
##                model = "factor.bayes", data = swiss, factors = 2,
##                lambda.constraints = list(Exam = list(1,"+"),
##                    Exam = list(2,"-"), Educ = c(2, 0),
##                    InfMort = c(1, 0)),
##                verbose = TRUE, a0 = 1, b0 = 0.15,
##                burnin = 5000, mcmc = 50000)
## geweke.diag(z.out$coefficients)
## heidel.diag(z.out$coefficients)
## raftery.diag(z.out$coefficients)
## summary(z.out)


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(cbind(Y1, Y2, Y3), model = "factor.bayes", data)


