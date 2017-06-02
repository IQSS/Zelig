# REQUIRE TEST probit mc -------------------------------------------------------
test_that("REQUIRE TEST probit mc", {
    z <- zprobit$new()
    test.probit <- z$mcunit(plot = FALSE)
    expect_true(test.probit)
})

# REQUIRE TEST probit example --------------------------------------------------
test_that("REQUIRE TEST probit example", {
    data(turnout)
    z.out <- zelig(vote ~ race + educate, model = "probit", data = turnout)
    x.out <- setx(z.out)
    s.out <- sim(z.out, x = x.out)
    expect_equal(sort(unique(zelig_qi_to_df(s.out)$predicted_value)), c(0, 1))
})

# REQUIRE TEST probit to_zelig ------------------------------------------------
test_that('REQUIRE TEST probit example', {
    data(turnout)
    m1 <- glm(vote ~ race + educate, family = binomial("probit"),
              data = turnout)
    m1.out <- setx(m1)
    m1.out <- sim(m1.out)
    expect_equal(sort(unique(zelig_qi_to_df(m1.out)$predicted_value)), c(0, 1))
    expect_error(plot(sim(setx(m1))), NA)
})
