# REQUIRE TEST Monte Carlo poisson ---------------------------------------------
test_that('REQUIRE TEST Monte Carlo poisson', {
    set.seed("123")
    z <- zpoisson$new()
    test.poisson <- z$mcunit(minx = 0, plot = FALSE)
    expect_true(test.poisson)
})

# REQUIRE TEST poisson example -------------------------------------------------
test_that('REQUIRE TEST poisson example', {
    data(sanction)
    z.out <- zelig(num ~ target + coop, model = "poisson", data = sanction)
    x.out <- setx(z.out)
    s.out <- sim(z.out, x = x.out)
    expect_error(s.out$graph(), NA)
})

# REQUIRE TEST poisson get_pvalue -------------------------------------------------
test_that('REQUIRE TEST poisson example', {
  data(sanction)
  z.out <- zelig(num ~ target + coop, model = "poisson", data = sanction)
  expect_error(z.out$get_pvalue(), NA)
})