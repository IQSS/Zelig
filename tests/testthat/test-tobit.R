# REQUIRE TEST Monte Carlo test tobit ---------------------------------------------

test_that('REQUIRE TEST tobit Monte Carlo', {
    z <- ztobit$new()
    test.tobit <- z$mcunit(minx = 0, plot = FALSE)
    expect_true(test.tobit)
})

# REQUIRE TEST update tobit formula --------------------------------------------
test_that('REQUIRE TEST update tobit formula', {
    data(tobin)
    z5<-ztobit$new()
    z5$zelig(durable ~ age + quant, data = tobin)

    z5.1_coefs <- coef(z5)

    controls <- ~ quant
    z5$zelig(formula = update(controls, durable ~ age + .), data = tobin)

    expect_equal(z5.1_coefs, coef(z5))
})

