# Integration tests for the Zelig estimate, set, sim, plot workflow ------------


# FAIL TEST sim workflow -------------------------------------------------------
test_that('FAIL TEST sim method warning if missing Zelig object', {
    z5 <- zls$new()
    z5$zelig(Fertility ~ Education, data = swiss)
    
    expect_warning(z5$sim())
})

# REQUIRE TEST sim custom num --------------------------------------------------
testthat('REQUIRE TEST sim custom num in sim', {
    z5 <- zls$new()
    z5$zelig(Fertility ~ Education, data = swiss)
    z5$setx(Education = 15)
    z5$sim()
  
    z5$sim.out
})