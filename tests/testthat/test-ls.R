# REQUIRE TEST Monte Carlo test ls ---------------------------------------------

test_that('REQUIRE TEST ls Monte Carlo', {
    z <- zls$new()
    test <- z$mcunit(plot = FALSE)
    expect_true(test)
})

# REQUIRE TEST ls with continuous covar -----------------------------------------

test_that('REQUIRE TEST ls continuous covar -- quickstart (Zelig 5 syntax)', {
    z5 <- zls$new()
    z5$zelig(Fertility ~ Education, data = swiss)

    # extract education coefficient parameter estimate and compare to reference
    expect_equivalent(round(as.numeric(z5$getcoef()[[1]][2]), 7), -0.8623503)
})


# REQUIRE TEST ls with by -------------------------------------------------------

test_that('REQUIRE TEST ls with by', {
    # Majority Catholic dummy
    swiss$maj_catholic <- cut(swiss$Catholic, breaks = c(0, 51, 100))

    z5 <- zls$new()
    z5$zelig(Fertility ~ Education, data = swiss, by = 'maj_catholic')
})

# gim method tests -------------------------------------------------------------

#test_that('REQUIRE TESTls gim method', {
    #z5$gim()
#})
