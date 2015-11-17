z <- zpoisson$new()
test <- z$mcunit(minx=0, plot=FALSE)
expect_true(test)