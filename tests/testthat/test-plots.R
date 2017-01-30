
# FAIL TEST ci.plot if simrange is not supplied --------------------------------
test_that('FAIL TEST ci.plot if simrange is not supplied', {
    z <- zls$new()
    z$zelig(Fertility ~ Education, data = swiss)

    expect_error(ci.plot(z),
                 'Simulations for a range of fitted values are not present.')
})

# FAIL TEST ci.plot first difference setrange and setrange1 same length --------
test_that('FAIL TEST ci.plot first difference setrange and setrange1 same length', {
    z <- zls$new()
    z$zelig(Fertility ~ Education, data = swiss)
    z$setrange(Education = 5:15)
    z$setrange1(Education = 10:11)
    z$sim()

    expect_error(z$graph(), 'The two fitted data ranges are not the same length.')

    # REQUIRE TEST for first difference over a range plots
    z <- zls$new()
    z$zelig(Fertility ~ Education, data = swiss)
    z$setrange(Education = 5:15)
    z$setrange1(Education = 15:25)
    z$sim()
    expect_error(z$graph(), NA)
})
