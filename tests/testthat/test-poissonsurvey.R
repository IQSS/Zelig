# REQUIRE TEST Monte Carlo test poissonsurvey ---------------------------------------------

test_that('REQUIRE TEST poissonsurvey Monte Carlo', {
    z <- zpoissonsurvey$new()
    test <- z$mcunit(plot = FALSE)
    expect_true(test)
})