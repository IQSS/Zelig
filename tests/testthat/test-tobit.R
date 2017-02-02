# REQUIRE TEST Monte Carlo test tobit ---------------------------------------------

test_that('REQUIRE TEST tobit Monte Carlo', {
    z <- ztobit$new()
    test.tobit <- z$mcunit(minx=0, plot = FALSE)
    expect_true(test.tobit)
})