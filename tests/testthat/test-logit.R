z <- zlogit$new()
test <- z$mcunit(minx=-4, maxx=4, plot=FALSE)
expect_true(test)
