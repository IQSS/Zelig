# REQUIRE TEST Monte Carlo test exp ---------------------------------------------

test_that('REQUIRE TEST exp Monte Carlo', {
    z <- zexp$new()
    test.exp <- z$mcunit(plot = FALSE)
    expect_true(test.exp)
})