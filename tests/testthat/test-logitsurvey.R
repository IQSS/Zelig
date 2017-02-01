# REQUIRE TEST Monte Carlo test logitsurvey ---------------------------------------------

test_that('REQUIRE TEST logitsurvey Monte Carlo', {
    z <- zlogitsurvey$new()
    test.logitsurvey <- z$mcunit(plot = FALSE)
    expect_true(test.logitsurvey)
})