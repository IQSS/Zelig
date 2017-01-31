# REQUIRE TEST Monte Carlo test logitbayes ---------------------------------------------

test_that('REQUIRE TEST logitbayes Monte Carlo', {
    z <- zlogitbayes$new()
    test <- z$mcunit(plot = FALSE)
    expect_true(test)
})