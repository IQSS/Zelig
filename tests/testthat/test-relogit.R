# REQUIRE TEST Monte Carlo test relogit ----------------------------------------

test_that('REQUIRE TEST relogit Monte Carlo', {
    z <- zrelogit$new()
    test.relogit <- z$mcunit(alpha = 0.1, b0 = -4, nsim = 1000, plot = FALSE)
    expect_true(test.relogit)
})


# REQUIRE TEST relogit vignette example ------------------------------------------------

test_that('REQUIRE TEST relogit vignette example', {
    data(mid)
    z.out1 <- zelig(conflict ~ major + contig + power + maxdem + mindem + years,
                    data = mid, model = "relogit", tau = 1042/303772)
    x.out1 <- setx(z.out1)
    s.out1 <- sim(z.out1, x = x.out1)
    sims <- zelig_qi_to_df(s.out1)

    expect_lt(mean(sims$predicted_value), 0.1)
})

# REQUIRE TEST relogit vignette logs transformation ----------------------------

test_that('REQUIRE TEST relogit vignette example', {
    data(mid)
    z.out1 <- zelig(conflict ~ major + contig + power + maxdem + mindem + years,
                    data = mid, model = "relogit", tau = 1042/303772)

    z.outlog <- zelig(conflict ~ major + contig + log(power) + maxdem + mindem +
                        years,
                    data = mid, model = "relogit", tau = 1042/303772)
    x.outlog <- setx(z.outlog, power = log(0.5))

    expect_false(coef(x.outlog)['power'] == coef(z.out1)['power'])
})

# FAIL TEST relogit with tau <= 0 ----------------------------------------------
test_that('FAIL TEST relogit with tau <= 0', {
    data(mid)
    expect_error(zelig(conflict ~ major + contig + power + maxdem + mindem +
                           years,
                    data = mid, model = "relogit", tau = -0.1),
                 "tau is the population proportion of 1's for the response variable.\nIt must be > 0.")
})

# REQUIRE TEST relogit with tau range ------------------------------------------
test_that('REQUIRE TEST relogit with tau range', {
    data(mid)
    expect_error(z.out <- zelig(conflict ~ major + contig + power + maxdem +
                                    mindem + years,
                    data = mid, model = "relogit", tau = c(0.002, 0.005)),
                 "tau must be a vector of length less than or equal to 1. For multiple taus, estimate models individually.")
})

# REQUIRE TEST relogit works with predict --------------------------------------
test_that("REQUIRE TEST relogit works with predict", {
    data(mid)
    x <- zelig(conflict ~ major, data = mid, model = "relogit",
               tau = 1042/303772)
    x <- from_zelig_model(x)
    expect_warning(predict(x, newdata = mid[1, ]), NA)
})

# REQUIRE TEST relogit follows ISQ (2001, eq. 11) ------------------------------
test_that("REQUIRE TEST relogit follows ISQ (2001, eq. 11)", {
    data(mid)
    z.out1 <- zelig(conflict ~ major + contig + power + maxdem + mindem + years,
                    data = mid, model = "relogit", tau = 1042/303772,
                    cite = FALSE, case.control = "weighting")
    expect_equal(round(coef(z.out1)[[2]], 6), 1.672177)
    expect_equal(colnames(summary(z.out1)$coefficients)[2],
                     "Std. Error (robust)")

    vcov_z.out1 <- vcov(z.out1)
    z.out.vcov_not_robust <- z.out1
    z.out.vcov_not_robust$robust.se <- FALSE
    expect_false(round(vcov_z.out1[[1]][1]) ==
                     round(vcov(z.out.vcov_not_robust)[[1]][1]))

    # Not adequately tested !!!
    z.out1 %>% setx() %>% sim() %>% plot()
    z.out.vcov_not_robust %>% setx() %>% sim() %>% plot()
})

# REQUIRE TEST Odds Ratio summary ----------------------------------------------
test_that('REQUIRE TEST Odds Ratio summary', {
    data(mid)
    z.out1 <- zelig(conflict ~ major + contig + power + maxdem + mindem + years,
                    data = mid, model = "relogit", tau = 1042/303772,
                    cite = FALSE, case.control = "weighting")

    sum_weighting <- summary(z.out1, odds_ratios = FALSE)
    sum_or_weighting <- summary(z.out1, odds_ratios = TRUE)
    expect_false(sum_weighting$coefficients[1, 1] ==
                     sum_or_weighting$coefficients[1, 1])
    expect_equal(colnames(sum_or_weighting$coefficients)[2],
                 "Std. Error (OR, robust)")

    z.out2 <- zelig(conflict ~ major + contig + power + maxdem + mindem + years,
                    data = mid, model = "relogit", tau = 1042/303772,
                    cite = FALSE, case.control = "prior")

    sum_weighting2 <- summary(z.out2, odds_ratios = FALSE)
    sum_or_weighting2 <- summary(z.out2, odds_ratios = TRUE)
    expect_equal(colnames(sum_or_weighting2$coefficients)[2],
                 "Std. Error (OR)")
})

# REQUIRE TEST get_predict takes type = "response" ----------------------------
test_that('REQUIRE TEST get_predict takes type = "response"', {
    data(mid)
    z.out1 <- zelig(conflict ~ major + contig + power + maxdem + mindem + years,
                    data = mid, model = "relogit", tau = 1042/303772)

    prob1 <- z.out1$get_predict(type = "response")
    expect_gt(min(sapply(prob1, min)), 0)

    prob2 <- predict(z.out1, type = "response")
    expect_gt(min(sapply(prob2, min)), 0)
})
