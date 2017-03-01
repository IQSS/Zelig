# REQUIRE TEST Monte Carlo test poissonsurvey ---------------------------------------------

test_that('REQUIRE TEST poissonsurvey Monte Carlo', {
    set.seed("123")
    z <- zpoissonsurvey$new()
    test.poissonsurvey <- z$mcunit(plot = FALSE)
    expect_true(test.poissonsurvey)
})