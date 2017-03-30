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
