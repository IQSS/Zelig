# REQUIRE TEST Monte Carlo test relogit ----------------------------------------

test_that('REQUIRE TEST relogit Monte Carlo', {
    set.seed(123)
    z <- zrelogit$new()
    test.relogit <- z$mcunit(alpha = 0.1, b0 = -4, nsim = 1000, plot = FALSE)
    expect_true(test.relogit)
})


# REQUIRE TEST relogit vignette example ------------------------------------------------

test_that('REQUIRE TEST relogit vignette example', {
    data(mid)
    z.out1 <- zelig(conflict ~ major + contig + power + maxdem + mindem + years,
                    data = mid, model = "relogit", tau = 1042/303772)
})

