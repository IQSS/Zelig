# REQUIRE TEST Monte Carlo test logitbayes -------------------------------------

test_that('REQUIRE TEST logitbayes Monte Carlo', {
    z <- zlogitbayes$new()
    test.logitbayes <- z$mcunit(nsim = 2000, ci = 0.99, plot = FALSE)
    expect_true(test.logitbayes)
})
