# REQUIRE TEST survey weights correctly passed  --------------------------------

test_that('REQUIRE TEST survey weights correctly passed', {
    data(api, package = "survey")

    z.out1 <- zelig(api00 ~ meals + yr.rnd, model = "normal.survey",
                    id = ~dnum, weights = 'pw', data = apiclus1, fpc = ~fpc)

    z.out2 <- zelig(api00 ~ meals + yr.rnd,
                    model = "normal.survey",
                    id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)

    z.out3 <- zelig(api00 ~ meals + yr.rnd, model = "normal.survey",
                    id = ~dnum, weights = apiclus1$pw, data = apiclus1,
                    fpc = ~fpc)

    api_design <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1,
                            fpc = ~fpc )
    model_glm <- svyglm(api00 ~ meals + yr.rnd, api_design,
                        family = gaussian("identity"))

    expect_equal(coef(z.out1), coef(z.out2))
    expect_equal(coef(z.out1), coef(z.out3))
    expect_equal(coef(z.out1), coef(model_glm))
})

# REQUIRE TEST survey weights correctly passed  --------------------------------

test_that('REQUIRE TEST survey glm with no weights', {
    data(api, package = "survey")

    z.out1_no_weights <- zelig(api00 ~ meals + yr.rnd, model = "normal.survey",
                    id = ~dnum, data = apiclus1, fpc = ~fpc)

    api_design_no_weights <- svydesign(id = ~dnum, data = apiclus1, fpc = ~fpc,
                                       weights = ~pw )
    model_glm_no_weights <- svyglm(api00 ~ meals + yr.rnd,
                                   api_design_no_weights,
                                   family = gaussian("identity"))

    expect_equal(coef(z.out1_no_weights), coef(model_glm_no_weights))
})


# REQUIRE TEST repweights ------------------------------------------------------
test_that('REQUIRE TEST repweights', {
    ### ----- NEED TO THINK OF A BETTER TEST ------ ##
    data(scd, package = "survey")

    BRRrep <- 2*cbind(c(1,0,1,0,1,0), c(1,0,0,1,0,1),
                      c(0,1,1,0,0,1), c(0,1,0,1,1,0))

    z.outREP <- zelig(alive ~ arrests , model = "normal.survey",
                    repweights = BRRrep, type = "BRR",
                    data = scd, na.action = NULL)
})
