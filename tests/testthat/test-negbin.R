
# REQUIRE TEST Monte Carlo test negbin ---------------------------------------------

test_that('REQUIRE TEST negbin Monte Carlo', {
    set.seed(123)
    z <- znegbin$new()
    test.negbin <- z$mcunit(plot=FALSE)
    expect_true(test.negbin)
})
