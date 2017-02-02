#### Integration tests for the Zelig estimate, set, sim, plot workflow      ####


# FAIL TEST sim workflow -------------------------------------------------------
test_that('FAIL TEST sim method warning if insufficient inputs', {
    z5 <- zls$new()
    z5$zelig(Fertility ~ Education, data = swiss)

    expect_warning(z5$sim(),
                   'No simulations drawn, likely due to insufficient inputs.')

    expect_error(z5$graph(), 'No simulated quantities of interest found.')
})

# FAIL TEST ci.plot range > length = 1 -----------------------------------------
test_that('FAIL TEST ci.plot range > length = 1', {
    z <- zls$new()
    z$zelig(Fertility ~ Education, data = swiss)
    expect_warning(z$setrange(Education = 5),
                   'Only one fitted observation provided to setrange.\nConsider using setx instead.')

    z$sim()
    expect_error(z$graph(),
                 'Simulations for more than one fitted observation are required.')

    expect_warning(z$setrange1(Education = 5),
                   'Only one fitted observation provided to setrange.\nConsider using setx instead.')
    expect_error(z$graph(),
                 'Simulations for more than one fitted observation are required.')
})

# REQUIRE TEST for by estimation workflow --------------------------------------
test_that('REQUIRE TEST for by estimation workflow', {
    # Majority Catholic dummy
    swiss$maj_catholic <- cut(swiss$Catholic, breaks = c(0, 51, 100))

    z5 <- zls$new()
    z5$zelig(Fertility ~ Education, data = swiss, by = 'maj_catholic')
    z5$setrange(Education = 5:15)
    z5$sim()

    z5$graph()

})

# FAIL TEST for estimation model failure ---------------------------------------
test_that('FAIL TEST for estimation model failure', {
    no_vary_df <- data.frame(y = rep(1, 10), x = rep(2, 10))
    z <- zarima$new()
    expect_error(z$zelig(y ~ x, data = no_vary_df),
                 'Dependent variable does not vary for at least one of the cases.')
    expect_error(summary(z), 'Zelig model has not been estimated.')
})

# REQUIRE TEST for sim num argument --------------------------------------------
test_that('REQUIRE TEST for sim num argument', {
    z5 <- zls$new()
    z5$zelig(Fertility ~ Education, data = swiss)
    z5$setx(Education = 5)

    z5$sim()
    expect_equal(length(z5$getqi()), 1000)

    z5$sim(num = 10) # Look into unexpected behaviour if sim order is reversed
    expect_equal(length(z5$getqi()), 10)
})

# REQUIRE TEST from_zelig returns expected fitted model object -----------------
test_that('REQUIRE TEST from_zelig returns expected fitted model object', {
    z5 <- zls$new()
    z5$zelig(Fertility ~ Education, data = swiss)
    expect_is(z5$from_zelig(), class = 'lm')
})

# REQUIRE TEST from_zelig returns each fitted model object from mi -------------
test_that('REQUIRE TEST from_zelig returns each fitted model object from mi', {
  set.seed(123)
  n <- 100
  x1 <- runif(n)
  x2 <- runif(n)
  y <- rnorm(n)
  data.1 <- data.frame(y = y, x = x1)
  data.2 <- data.frame(y = y, x = x2)
  
  mi.out <- to_zelig_mi(data.1, data.2)
  z.out <- zelig(y ~ x, model = "ls", data = mi.out)
  expect_is(z.out$from_zelig(), 'list')
})
