z <- zlognorm$new()
test.lognorm <- z$mcunit(minx=0, ci=0.99, nsim=1000, plot=FALSE)
expect_true(test.lognorm)