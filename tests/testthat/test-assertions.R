# FAIL TESTS no Zelig model included -------------------------------------------
test_that('FAIL TEST setx method error if missing Zelig model estimation', {
    z5 <- zls$new()

    expect_error(z5$setx(), 'Zelig model has not been estimated.')
})

test_that('FAIL TEST setrange method error if missing Zelig model estimation', {
    z5 <- zls$new()

    expect_error(z5$setrange(), 'Zelig model has not been estimated.')
})

test_that('FAIL TEST sim method error if missing Zelig model estimation', {
    z5 <- zls$new()

    expect_error(z5$sim(), 'Zelig model has not been estimated.')
})

test_that('FAIL TEST graph method error if missing Zelig model estimation', {
    z5 <- zls$new()

    expect_error(z5$graph(), 'Zelig model has not been estimated.')
})

# FAIL TEST insufficient inputs for sim ----------------------------------------
test_that('FAIL TEST sim method error if missing Zelig model estimation', {
    z5 <- zls$new()

    expect_error(z5$sim(), 'Zelig model has not been estimated.')
})

# FAIL TEST length is not greater than 1 ---------------------------------------
test_that('FAIL TEST length is not greater than 1', {
    not_more_1 <- 1
    expect_error(is_length_not_1(not_more_1), 'Length is 1.')
})

# FAIL TEST vector does not vary -----------------------------------------------
test_that('FAIL TEST vector does not vary', {
    expect_error(is_varying(c(rep(1, 5))), 'Vector does not vary.')
})

# REQUIRE TEST vector does not vary --------------------------------------------
test_that('REQIURE TEST vector does not vary', {
    expect_true(is_varying(c(1, 2, 3), fail = FALSE))
})

# FAIL TEST is_simsx error message ---------------------------------------------
test_that('FAIL TEST is_simsx error message', {
    z <- zls$new()
    expect_error(is_simsx(z$sim.out), 
                 'Simulations for individual fitted values are not present.')
})

# FAIL TEST is_timeseries ------------------------------------------------------
test_that('FAIL TEST is_timeseries', {
    z <- zls$new()
    expect_false(is_timeseries(z))
    expect_error(is_timeseries(z, fail = TRUE), 'Not a timeseries object.')
}) 
