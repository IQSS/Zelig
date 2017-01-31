# REQUIRE TEST Monte Carlo test tobit ---------------------------------------------

test_that('REQUIRE TEST tobit Monte Carlo', {
    z <- ztobit$new()
    test <- z$mcunit(plot = FALSE)
    expect_true(test)
})