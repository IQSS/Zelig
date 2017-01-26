# REQUIRE TEST Monte Carlo test logit ------------------------------------------

test_that('REQUIRE TEST logit Monte Carlo', {
    z <- zlogit$new()
    test <- z$mcunit(minx = -2, maxx = 2, plot = FALSE)
    expect_true(test)
})
