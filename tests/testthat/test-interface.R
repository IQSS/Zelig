# REQUIRE TEST from_zelig_model returns expected fitted model object -----------
test_that('REQUIRE TEST from_zelig_model returns expected fitted model object', {
    z5 <- zls$new()
    z5$zelig(Fertility ~ Education, data = swiss)
    expect_is(from_zelig_model(z5), class = 'lm')
})


# REQUIRE TEST zelig_qi_to_df setx, setrange, by --------------- ---------------
test_that('REQUIRE TEST zelig_qi_to_df setx, setrange, by', {
    #### QIs without first difference or range, from covariates fitted at
    ## central tendencies
    z.1 <- zelig(Petal.Width ~ Petal.Length + Species, data = iris,
                 model = "ls")
    z.1 <- setx(z.1)
    z.1 <- sim(z.1)
    expect_equal(nrow(zelig_qi_to_df(z.1)), 1000) 

    #### QIs for first differences
    z.2 <- zelig(Petal.Width ~ Petal.Length + Species, data = iris,
                 model = "ls")
    z.2a <- setx(z.2, Petal.Length = 2)
    z.2b <- setx(z.2, Petal.Length = 4.4)
    z.2 <- sim(z.2, x = z.2a, x1 = z.2a)
    z2_extracted <- zelig_qi_to_df(z.2)
    expect_equal(nrow(z2_extracted), 2000) 
    expect_equal(ncol(z2_extracted), 6) 

    #### QIs for first differences, estimated by Species
    z.3 <- zelig(Petal.Width ~ Petal.Length, by = "Species", data = iris,
                 model = "ls")
    z.3a <- setx(z.3, Petal.Length = 2)
    z.3b <- setx(z.3, Petal.Length = 4.4)
    z.3 <- sim(z.3, x = z.3a, x1 = z.3a)
    expect_equal(nrow(zelig_qi_to_df(z.3)), 6000) 

    #### QIs for a range of fitted values
    z.4 <- zelig(Petal.Width ~ Petal.Length + Species, data = iris,
                 model = "ls")
    z.4 <- setx(z.4, Petal.Length = 2:4)
    z.4 <- sim(z.4)
    z4_extracted <- zelig_qi_to_df(z.4)
    expect_equal(nrow(z4_extracted), 3000)
    expect_is(z4_extracted, class = 'data.frame')

    #### QIs for a range of fitted values, estimated by Species
    z.5 <- zelig(Petal.Width ~ Petal.Length, by = "Species", data = iris,
                model = "ls")
    z.5 <- setx(z.5, Petal.Length = 2:4)
    z.5 <- sim(z.5)
    z5_extracted <- zelig_qi_to_df(z.5)
    expect_equal(nrow(z5_extracted), 9000)
    expect_equal(names(z5_extracted), c('setx_value', 'by', 'Petal.Length', 
                                        'expected_value', 'predicted_value'))

    #### QIs for two ranges of fitted values
    z.6 <- zelig(Petal.Width ~ Petal.Length + Species, data = iris,
                model = "ls")
    z.6a <- setx(z.6, Petal.Length = 2:4, Species = 'setosa')
    z.6b <- setx(z.6, Petal.Length = 2:4, Species = 'virginica')
    z.6 <- sim(z.6, x = z.6a, x1 = z.6b)

    expect_equal(nrow(zelig_qi_to_df(z.6)), 6000)
})
