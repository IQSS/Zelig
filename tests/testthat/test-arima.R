# REQUIRE TEST arima Monte Carlo -----------------------------------------------

## Need to implement ##

# REQUIRE TEST arima successful estimation -------------------------------------
test_that('REQUIRE TEST arima successful estimation', {
    data(seatshare)
    ts <- zarima$new()

    ## NEEDS a better test, possibly once getcoef has been implemented for arima
    expect_error(
    ts$zelig(unemp ~ leftseat, order = c(1,0,1), ts = "year", cs = "country",
              data = seatshare),
              NA)
})

# FAIL TEST arima fails if DV does not vary ------------------------------------
test_that('FAIL TEST arima fails if DV does not vary', {
    no_vary_df <- data.frame(country = c(rep("A", 5), rep("B", 5)),
                             year = c(1:5, 1:5),
                             y = c(rep(1:5), rep(2, 5)),
                         x = c(1, 3, -1, NA, 1, NA, 1, 2, NA, 5))
   # a.out <- amelia(x = no_vary_df, cs = "country", ts = "year")

    zts <- zarima$new()
    expect_error(
        zts$zelig(y ~ x, ts = 'year', cs = 'country', order = c(1, 0, 1),
            data = no_vary_df),
            'Dependent variable does not vary for at least one of the cases.')
})
