# REQUIRE TEST Bayes Diagnostics ---------------------------------------------

test_that('REQUIRE TEST Bayes Diagnostics', {
    set.seed("123")
    data(macro)
    expect_error(zelig(unem ~ gdp + capmob + trade, model = "normal.bayes",
                       bootstrap = 100, data = macro),
                 "Error: The bootstrap is not available for Markov chain Monte Carlo (MCMC) models.", fixed=TRUE)
    z <- zelig(unem ~ gdp + capmob + trade, model = "normal.bayes", data = macro, verbose = FALSE)
    geweke.test <- z$geweke.diag()
    heidel.test <- z$heidel.diag()
    raftery.test <- z$raftery.diag()
    expect_equivalent(length(geweke.test),2)
    expect_equivalent(length(heidel.test),30)
    expect_equivalent(length(raftery.test),2)
})

test_that('REQUIRE TEST Bayes Diagnostics for factors', {
    set.seed("123")
    data(swiss)
    names(swiss) <- c("Fert", "Agr", "Exam", "Educ", "Cath", "InfMort")
    z <- zelig(~ Agr + Exam + Educ + Cath + InfMort,
               model = "factor.bayes", data = swiss,
               factors = 2, verbose = FALSE,
               a0 = 1, b0 = 0.15, burnin = 500, mcmc = 5000)
    geweke.test <- z$geweke.diag()
    heidel.test <- z$heidel.diag()
    raftery.test <- z$raftery.diag()
    expect_equivalent(length(geweke.test),2)
    expect_equivalent(length(heidel.test),90)
    expect_equivalent(length(raftery.test),2)
})
