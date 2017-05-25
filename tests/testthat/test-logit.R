# REQUIRE TEST Monte Carlo test logit ------------------------------------------

test_that('REQUIRE TEST logit Monte Carlo', {
    z <- zlogit$new()
    test <- z$mcunit(minx = -2, maxx = 2, plot = FALSE)
    expect_true(test)
})

# REQUIRE TEST logit example and show odds_ratios ------------------------------
test_that('REQUIRE TEST logit example and show odds_ratios', {
    data(turnout)
    z.out1 <- zelig(vote ~ age + race, model = "logit", data = turnout,
                    cite = FALSE)

    betas <- coef(z.out1)
    ors <- summary(z.out1, odds_ratios = TRUE)
    ors <- ors$summ[[1]]$coefficients[1:3]

    expect_equal(exp(betas)[[1]], ors[1])
})
