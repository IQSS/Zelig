# Monte Carlo test -------------------------------------------------------------

test_that('REQUIRE TEST ls monte carlo test', {
    z <- zls$new()
    test <- z$mcunit(plot = FALSE)
    expect_true(test)
})

# Documentation examples -------------------------------------------------------

test_that('REQUIRE TEST ls continuous covar -- quickstart (Zelig 5 syntax)', {
    z5 <- zls$new()
    z5$zelig(Fertility ~ Education, data = swiss)
  
    # extract education coefficient parameter estimate and compare to reference
    expect_equivalent(round(as.numeric(z5$getcoef()[[1]][2]), 7), -0.8623503)
})

# gim method tests -------------------------------------------------------------

#test_that('REQUIRE TESTls gim method', {
    #z5$gim()
#})