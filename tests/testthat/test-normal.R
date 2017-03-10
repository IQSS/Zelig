# REQUIRE TEST Monte Carlo test normal ---------------------------------------------

test_that('REQUIRE TEST normal Monte Carlo', {
    set.seed(123)
    z <- znormal$new()
    test.normal <- z$mcunit(plot = FALSE)
    expect_true(test.normal)
})
