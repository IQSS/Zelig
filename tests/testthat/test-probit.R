z <- zprobit$new()
test.probit <- z$mcunit(plot=FALSE)
expect_true(test.probit)