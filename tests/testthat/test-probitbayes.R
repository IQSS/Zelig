# REQUIRE TEST Monte Carlo test probitbayes ---------------------------------------------

test_that('REQUIRE TEST probitbayes Monte Carlo', {
    z <- zprobitbayes$new()
    test <- z$mcunit(nsim=1000, plot = FALSE)
    expect_true(test)
})