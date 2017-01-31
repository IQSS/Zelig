# REQUIRE TEST Monte Carlo test probitsurvey ---------------------------------------------

test_that('REQUIRE TEST probitsurvey Monte Carlo', {
    z <- zprobitsurvey$new()
    test <- z$mcunit(plot = FALSE)
    expect_true(test)
})