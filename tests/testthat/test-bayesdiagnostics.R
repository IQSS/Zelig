# REQUIRE TEST Bayes Diagnostics ---------------------------------------------

test_that('REQUIRE TEST Bayes Diagnostics', {
	data(macro)
	expect_error(zelig(unem ~ gdp + capmob + trade, model = "normal.bayes", bootstrap=100, data = macro),
                 "Error: The bootstrap is not available for Markov chain Monte Carlo (MCMC) models.", fixed=TRUE)
	z <- zelig(unem ~ gdp + capmob + trade, model = "normal.bayes", data = macro, verbose = FALSE)
    geweke.test <- z$geweke.diag()
    heidel.test <- z$heidel.diag()
    raftery.test <- z$raftery.diag()
    expect_equivalent(length(geweke.test),2)
    expect_equivalent(length(heidel.test),30)
    expect_equivalent(length(raftery.test),2)
})