# REQUIRE TEST Monte Carlo test normalsurvey ---------------------------------------------

test_that('REQUIRE TEST normalsurvey Monte Carlo', {
    z <- znormalsurvey$new()
    test.normalsurvey <- z$mcunit(plot = FALSE)
    expect_true(test.normalsurvey)
})