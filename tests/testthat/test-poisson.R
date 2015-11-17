z <- zpoisson$new()
test <- z$mcunit(plot=FALSE)
expect_true(test)