# REQUIRE TEST Monte Carlo test tobitbayes ---------------------------------------------

test_that('REQUIRE TEST tobitbayes Monte Carlo', {
    z <- ztobitbayes$new()
    test.tobitbayes <- z$mcunit(nsim=2000, ci=0.99, minx=0, plot = FALSE)
    expect_true(test.tobitbayes)
})