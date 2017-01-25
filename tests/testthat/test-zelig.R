# Integration tests for the Zelig estimate, set, sim, plot workflow ------------


# FAIL TEST sim workflow -------------------------------------------------------
test_that('FAIL TEST sim method warning if insufficient inputs', {
    z5 <- zls$new()
    z5$zelig(Fertility ~ Education, data = swiss)
    
    expect_warning(z5$sim(), 
                   'No simulations drawn, likely due to insufficient inputs.')
    
    expect_error(z5$graph(), 'No simulated quantities of interest found.')
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
