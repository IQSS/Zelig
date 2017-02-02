# REQUIRE TEST Monte Carlo test gammasurvey ---------------------------------------------

test_that('REQUIRE TEST gammasurvey Monte Carlo', {
    z <- zgammasurvey$new()
    test.gammasurvey <- z$mcunit(b0=1, b1=-0.6, alpha=3, minx=0, maxx=1, nsim=2000, ci=.99, plot = FALSE)
    expect_true(test.gammasurvey)
})