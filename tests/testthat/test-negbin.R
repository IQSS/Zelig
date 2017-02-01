z <- znegbin$new()
test.negbin <- z$mcunit(plot=FALSE)
expect_true(test.negbin)