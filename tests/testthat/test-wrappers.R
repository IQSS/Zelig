# Zelig 4 ls wrapper working ---------------------------------------------------

test_that('ls wrapper continuous covar -- quickstart (Zelig 4 syntax)', {
    z4 <- zelig(Fertility ~ Education, data = swiss, model = 'ls', cite = FALSE)

    # extract education coefficient parameter estimate and compare to reference
    expect_equivalent(round(as.numeric(z4$get_coef()[[1]][2]), 7), -0.8623503)
})

# Test missing model argument error---------------------------------------------

test_that('missing model argument error', {
    expect_error(zelig(Fertility ~ Education, data = swiss),
               'Estimation model type not specified.\nSelect estimation model type with the model argument.'
  )
})

# Test non-supported model type error ------------------------------------------

test_that('non-supported model type error', {
    expect_error(zelig(Fertility ~ Education, data = swiss, model = 'TEST'),
                 'TEST is not a supported model type'
  )
})

# REQUIRE TEST wrapper setx ----------------------------------------------------

test_that('REQUIRE TEST wrapper setx', {
    z4 <- zelig(Fertility ~ Education, data = swiss, model = 'ls')

    z4_set <- setx(z4)
    z4_set_vector <- round(as.vector(unlist(z4_set$setx.out)))
    expect_equivalent(z4_set_vector, c(1, 1, 11))
})

# REQUIRE TEST wrapper setx1 ----------------------------------------------------

test_that('REQUIRE TEST wrapper setx1', {
zpipe <- zelig(Fertility ~ Education, data = swiss, model = 'ls') %>%
                setx(z4, Education = 10) %>%
                setx1(z4, Education = 30) %>%
                sim()
    expect_equal(length(zpipe$sim.out), 2)

})

# FAIL TEST non-zelig objects --------------------------------------------------
test_that('setx and sim non-zelig object fail', {
    expect_error(setx('TEST'), 'Not a Zelig object and not convertible to one.')
    expect_error(sim('TEST'), 'Not a Zelig object.')
})

# REQUIRE TEST sim wrapper minimal working --------------------------------------
test_that('REQUIRE TEST sim wraper minimal working', {
    z5 <- zls$new()
    z5 <- zelig(Fertility ~ Education, data = swiss, model = 'ls')
    set_x <- setx(z5, Education = 5)

    zsimwrap <- sim(z5, x = set_x, num = 10)
    expect_equal(length(zsimwrap$get_qi()), 10)
    expect_equal(length(zsimwrap$get_qi()), length(get_qi(zsimwrap)))

    z5$setx(Education = 5)
    zsimwrap <- sim(z5, num = 10)
    expect_equal(length(zsimwrap$get_qi()), 10)
})

# REQUIRE TEST ATT wrapper -----------------------------------------------------
test_that('REQUIRE TEST ATT wrapper', {
    data(sanction)
    # no wrapper
    zqi.out <- zelig(num ~ target + coop + mil, model = "poisson",
                     data = sanction)
    zqi.out$ATT(treatment = "mil")
    my.att <- zqi.out$get_qi(qi = "ATT", xvalue = "TE")

    # with wrapper
    library(dplyr)

    z.att <- zelig(num ~ target + coop + mil, model = "poisson",
                   data = sanction) %>%
             ATT(treatment = "mil") %>%
             get_qi(qi = "ATT", xvalue = "TE")
    expect_equal(length(my.att), length(z.att))
})
