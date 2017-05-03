# REQUIRE TEST ivreg Monte Carlo -----------------------------------------------
#test_that("REQUIRE Test ivreg Monte Carlo", {
#    z <- zivreg$new()
#    test.ivreg <- z$mcunit(plot = FALSE)
#    expect_true(test.ivreg)
#})

# REQUIRE TEST ivreg AER example with log transformations ----------------------
test_that("REQUIRE TEST ivreg AER example with log transformations", {
    library(AER)
    # Example from AER (version 1.2-5) documentation
    data("CigarettesSW")
    CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
    CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
    CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)
    CigarettesSW1995 <- subset(CigarettesSW, year == 1995)

    # Unwrapped
    fm <- ivreg(log(packs) ~ log(rprice) + log(rincome) |
                log(rincome) + tdiff + I(tax/cpi),
                data = CigarettesSW1995)

    # Zelig wrapped
    CigarettesSW1995$log_rprice <- log(CigarettesSW1995$rprice)
    CigarettesSW1995$log_rincome <- log(CigarettesSW1995$rincome)
    ziv.out <- zelig(log(packs) ~ log_rprice + log_rincome |
                    log_rincome + tdiff + I(tax/cpi),
                    data = CigarettesSW1995,
                    model = 'ivreg')
    expect_equal(coef(fm)[[2]], coef(ziv.out)[[2]])
    expect_equivalent(vcov(fm), vcov(ziv.out)[[1]])
})

# REQUIRE TEST ivreg setx and sim ----------------------------------------------
test_that("REQUIRE TEST ivreg setx", {
    data("CigarettesSW")
    CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
    CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
    CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)
    CigarettesSW1995 <- subset(CigarettesSW, year == 1995)

    CigarettesSW1995$log_rprice <- log(CigarettesSW1995$rprice)
    CigarettesSW1995$log_rincome <- log(CigarettesSW1995$rincome)

    ziv.out <- zelig(log(packs) ~ log_rprice + log_rincome |
                log_rincome + tdiff + I(tax/cpi),
                data = CigarettesSW1995, model = 'ivreg')
    ziv.set <- setx(ziv.out, log_rprice = log(95:118))
    expect_equal(length(ziv.set$setx.out$range), 24)
    expect_error(sim(ziv.set), NA)

    expect_error(plot(sim(ziv.set)), NA)
})

# FAIL TEST ivreg with 2nd stage covariates logged in zelig call ---------------
test_that("FAIL TEST ivreg with 2nd stage covariates logged in zelig call", {
    data("CigarettesSW")
    CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
    CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
    CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)
    CigarettesSW1995 <- subset(CigarettesSW, year == 1995)

    expect_error(
    ziv.out <- zelig(log(packs) ~ log(rprice) + log(rincome) |
                         log(rincome) + tdiff + I(tax/cpi),
                     data = CigarettesSW1995, model = 'ivreg'),
    "logging values in the zelig call is not currently supported for ivreg models."
    )
})
