# Integration tests for the Zelig estimate, set, sim, plot workflow

test_that('FAIL TEST sim method warning if missing Zelig object', {
    z5 <- zls$new()
    z5$zelig(Fertility ~ Education, data = swiss)
    
    expect_warning(z5$sim())
})