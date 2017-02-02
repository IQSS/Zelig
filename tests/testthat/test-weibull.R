# REQUIRE TEST Monte Carlo weibull ---------------------------------------------

test_that('REQUIRE TEST weibull Monte Carlo', {
    z <- zweibull$new()
    test.weibull<-z$mcunit(minx = 2, maxx = 3, nsim = 2000, alpha = 1.5, b0 = -1, b1 = 2, ci = 0.99, plot = FALSE)
    expect_true(test.weibull)
})