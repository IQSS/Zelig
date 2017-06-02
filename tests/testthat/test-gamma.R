# REQUIRE TEST Monte Carlo test gamma ---------------------------------------------
test_that('REQUIRE TEST gamma Monte Carlo', {
    z <- zgamma$new()
    test.gamma <- z$mcunit(b0 = 1, b1 = -0.6, alpha = 3, minx = 0, maxx = 1,
                           nsim = 2000, ci = 0.99, plot = FALSE)
    expect_true(test.gamma)
})

# REQUIRE TEST gamma example ---------------------------------------------------
test_that('REQUIRE TEST gamma example', {
    data(coalition)
    z.out <- zelig(duration ~ fract + numst2, model = "gamma", data = coalition)
    expect_error(plot(sim(setx(z.out))), NA)
})

# REQUIRE TEST gamma to_zelig --------------------------------------------------
test_that('REQUIRE TEST gamma example', {
    data(coalition)
    m1 <- glm(duration ~ fract + numst2, family = Gamma(link="inverse"),
                   data = coalition)
    expect_message(setx(m1), 'Assuming zgamma to convert to Zelig.')
    expect_error(plot(sim(setx(m1))), NA)
})
