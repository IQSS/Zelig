# REQUIRE TEST Monte Carlo test tobitbayes ---------------------------------------------

test_that('REQUIRE TEST tobitbayes Monte Carlo', {
    z <- ztobitbayes$new()
    test <- z$mcunit(plot = FALSE)
    expect_true(test)
})