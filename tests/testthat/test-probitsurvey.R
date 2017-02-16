# REQUIRE TEST Monte Carlo test probitsurvey ---------------------------------------------

test_that('REQUIRE TEST probitsurvey Monte Carlo', {
    z <- zprobitsurvey$new()
    test.probitsurvey <- z$mcunit(minx = -1, maxx = 1, plot = FALSE)
    expect_true(test.probitsurvey)
})