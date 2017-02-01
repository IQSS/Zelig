# REQUIRE TEST Monte Carlo test poissonbayes ---------------------------------------------

test_that('REQUIRE TEST poissonbayes Monte Carlo', {
    z <- zpoissonbayes$new()
    test.poissonbayes <- z$mcunit(minx=1, nsim = 2000, ci=0.99, plot = FALSE)
    expect_true(test.poissonbayes)
})