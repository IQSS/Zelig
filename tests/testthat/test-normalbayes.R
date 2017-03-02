# REQUIRE TEST Monte Carlo test normalbayes ---------------------------------------------

test_that('REQUIRE TEST normalbayes Monte Carlo', {
	set.seed(123)
    z <- znormalbayes$new()
    test.normalbayes <- z$mcunit(minx=-1, maxx = 1, ci=0.99, nsim=2000, plot = TRUE)
    expect_true(test.normalbayes)
})