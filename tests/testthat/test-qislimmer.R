# REQUIRE TEST for qi_slimmer --------------------------------------------------
test_that('REQUIRE TEST for qi_slimmer', {
  qi.full.interval <- zelig(Petal.Width ~ Petal.Length + Species,
                            data = iris, model = "ls") %>%
    setx(Petal.Length = 2:4, Species = "setosa") %>%
    sim() %>%
    zelig_qi_to_df()

  expect_equal(nrow(qi_slimmer(qi.full.interval)), 3)
  expect_equal(nrow(qi_slimmer(qi.full.interval, qi_type = 'pv')), 3)
  expect_equal(nrow(qi_slimmer(qi.full.interval, ci = 90)), 3)
})

# FAIL TEST for qi_slimmer --------------------------------------------------
test_that('REQUIRE TEST for qi_slimmer', {
  qi.full.interval <- zelig(Petal.Width ~ Petal.Length + Species,
                            data = iris, model = "ls") %>%
    setx(Petal.Length = 2:4, Species = "setosa") %>%
    sim() %>%
    zelig_qi_to_df()

    expect_error(qi_slimmer(qi.full.interval, qi_type = 'TEST'))
    expect_error(qi_slimmer(qi.full.interval, ci = 900),
                '900 will not produce a valid central interval.')

    z <- zelig(Petal.Width ~ Petal.Length + Species, data = iris, model = "ls")
    expect_error(qi_slimmer(z),
                'df must be a data frame created by zelig_qi_to_df.')
    df_test <- data.frame(a = 1, b = 2)
    expect_error(qi_slimmer(df_test),
                 'The data frame does not appear to have been created by zelig_qi_to_df.')
})

