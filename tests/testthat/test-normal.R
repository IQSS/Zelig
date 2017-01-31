# REQUIRE TEST Monte Carlo test normal ---------------------------------------------

test_that('REQUIRE TEST normal Monte Carlo', {
    z <- znormal$new()
    test <- z$mcunit(plot = FALSE)
    expect_true(test)
})