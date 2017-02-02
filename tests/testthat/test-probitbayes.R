# REQUIRE TEST Monte Carlo test probitbayes ---------------------------------------------

test_that('REQUIRE TEST probitbayes Monte Carlo', {
    z <- zprobitbayes$new()
    test.probitbayes <- z$mcunit(minx=-1, maxx = 1, ci=0.99, nsim=2000, plot = FALSE)
    expect_true(test.probitbayes)
})