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

    expect_equal(coef(z.out1), coef(z.out2), coef(z.out3), coef(model_glm))



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


