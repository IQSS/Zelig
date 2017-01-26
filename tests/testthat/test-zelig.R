# Integration tests for the Zelig estimate, set, sim, plot workflow ------------


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
