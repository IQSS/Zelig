# REQUIRE TEST Monte Carlo test exp ---------------------------------------------

test_that('REQUIRE TEST exp Monte Carlo', {
    set.seed(123)
    z <- zexp$new()
    test.exp <- z$mcunit(plot = FALSE)
    expect_true(test.exp)
})

# REQUIRE TEST (minimal) documentation example -------------------------------------------

test_that('REQUIRE TEST (minimal) documentation example', {
    data(coalition)
    z.out <- zelig(Surv(duration, ciep12) ~ fract + numst2, model = "exp",
                   data = coalition)
    x.low <- setx(z.out, numst2 = 0)
    x.high <- setx(z.out, numst2 = 1)
    expect_error(sim(z.out, x = x.low, x1 = x.high), NA)
})
