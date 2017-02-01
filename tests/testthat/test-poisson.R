z <- zpoisson$new()
test.poisson <- z$mcunit(minx=0, plot=FALSE)
expect_true(test.poisson)