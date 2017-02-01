# REQUIRE TEST Monte Carlo test poissonsurvey ---------------------------------------------

test_that('REQUIRE TEST poissonsurvey Monte Carlo', {
    z <- zpoissonsurvey$new()
    test.poissonsurvey <- z$mcunit(plot = FALSE)
    expect_true(test.poissonsurvey)
})