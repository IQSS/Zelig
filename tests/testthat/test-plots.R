
# ci.plot FAIL TEST if simrange is not supplied --------------------------------
test_that('ci.plot FAIL TEST if simrange is not supplied', {
    z <- zls$new()
    z$zelig(Fertility ~ Education, data = swiss)
    
    expect_error(ci.plot(z), 
                 'Simulations for a range of fitted values are not present.')
})
