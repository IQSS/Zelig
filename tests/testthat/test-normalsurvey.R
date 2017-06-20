# REQUIRE TEST Monte Carlo test normalsurvey -----------------------------------
test_that('REQUIRE TEST normalsurvey Monte Carlo', {
    z <- znormalsurvey$new()
    test.normalsurvey <- z$mcunit(plot = FALSE)
    expect_true(test.normalsurvey)
})

# REQUIRE TEST to_zelig for normalsurvey ---------------------------------------
test_that('REQUIRE TEST to_zelig for normalsurvey', {
    data(api)
    dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat,
                      fpc = ~fpc)
    m1 <- svyglm(api00 ~ ell + meals + mobility, design = dstrat)
    expect_error(plot(sim(setx(m1))), NA)
})
