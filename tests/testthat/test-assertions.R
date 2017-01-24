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

# FAIL TEST for installed package assertion ------------------------------------
test_that('FAIL TEST for installed package assertion', {
    expect_error(is_pkginstalled('TEST'))
})
