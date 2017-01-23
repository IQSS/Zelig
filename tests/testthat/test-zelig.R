# Integration tests for the Zelig estimate, set, sim, plot workflow ------------


# FAIL TEST sim workflow -------------------------------------------------------
test_that('FAIL TEST sim method warning if insufficient inputs', {
    z5 <- zls$new()
    z5$zelig(Fertility ~ Education, data = swiss)
    
    expect_warning(z5$sim(), 
                   'No simulations drawn, likely due to insufficient inputs.')
    
    expect_error(z5$graph(), 'No simulated quantities of interest found.')
})

