# REQUIRE TEST ivreg Monte Carlo -----------------------------------------------
test_that("REQUIRE Test ivreg Monte Carlo", {
    z <- zivreg$new()
    test.ls <- z$mcunit(plot = FALSE)
    expect_true(test.ls)
})

# REQUIRE TEST ivreg AER example with log transformations ----------------------
test_that("REQUIRE TEST ivreg AER example with log transformations", {
    # Example from AER (version 1.2-5) documentation
    library(AER)
    data("CigarettesSW", package = "AER")
    CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
    CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
    CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)

    # Unwrapped
    fm <- ivreg(log(packs) ~ log(rprice) + log(rincome) |
                log(rincome) + tdiff + I(tax/cpi),
                data = CigarettesSW, subset = year == "1995")

    # Wrapped
    ziv.out <- zelig(log(packs) ~ log(rprice) + log(rincome) |
                log(rincome) + tdiff + I(tax/cpi),
                data = CigarettesSW, subset = year == "1995", model = 'ivreg')
    expect_equal(coef(fm), coef(ziv.out))
    expect_equivalent(vcov(fm), vcov(ziv.out)[[1]])
})

# REQUIRE TEST ivreg setx ------------------------------------------------------
test_that("REQUIRE TEST ivreg setx", {
    # Example from AER (version 1.2-5) documentation
    library(AER)
    data("CigarettesSW", package = "AER")
    CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
    CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
    CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)

    ziv.out <- zelig(packs ~ log(rprice) + log(rincome) |
                log(rincome) + tdiff + I(tax/cpi),
                data = CigarettesSW, subset = year == "1995", model = 'ivreg')
    ziv.set <- setx(ziv.out)

})
