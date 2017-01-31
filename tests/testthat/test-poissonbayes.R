# REQUIRE TEST Monte Carlo test poissonbayes ---------------------------------------------

test_that('REQUIRE TEST poissonbayes Monte Carlo', {
    z <- zpoissonbayes$new()
    test <- z$mcunit(plot = FALSE)
    expect_true(test)
})