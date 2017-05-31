# REQUIRE TEST Monte Carlo test ls ---------------------------------------------

test_that('REQUIRE TEST ls Monte Carlo', {
    z <- zls$new()
    test.ls <- z$mcunit(plot = FALSE)
    expect_true(test.ls)
})

# REQUIRE TEST ls with continuous covar -----------------------------------------

test_that('REQUIRE TEST ls continuous covar -- quickstart (Zelig 5 syntax)', {
    z5 <- zls$new()
    z5$zelig(Fertility ~ Education, data = swiss)

    # extract education coefficient parameter estimate and compare to reference
    expect_equivalent(round(as.numeric(z5$get_coef()[[1]][2]), 7), -0.8623503)
})


# REQUIRE TEST ls with by -------------------------------------------------------

test_that('REQUIRE TEST ls with by', {
    # Majority Catholic dummy
    swiss$maj_catholic <- cut(swiss$Catholic, breaks = c(0, 51, 100))

    z5by <- zls$new()
    z5by$zelig(Fertility ~ Education, data = swiss, by = 'maj_catholic')
    z5by$setx()
    z5by$sim()
    sims_df <- zelig_qi_to_df(z5by)
    expect_equal(length(unique(sims_df$by)), 2)
})

# REQUIRE TEST gim method ------------------------------------------------------

#test_that('REQUIRE TESTls gim method', {
    #z5$gim()
#})


# REQUIRE TEST for sim with ls models including factor levels ------------------
test_that('REQUIRE TEST for sim with models including factor levels', {
    expect_is(iris$Species, 'factor')
    z.out <- zelig(Petal.Width ~ Petal.Length + Species, data = iris,
                   model = "ls")
    x.out1 <- setx(z.out, Petal.Length = 1:10)
    sims1 <- sim(z.out, x.out1)
    expect_equal(length(sims1$sim.out$range), 10)

    x.out2 <- setx(z.out, Petal.Length = 1:10, fn = list(numeric = Median))
    sims2 <- sim(z.out, x.out2)
    expect_equal(length(sims2$sim.out$range), 10)
})

# REQUIRE TEST for set with ls models including factors set within zelig call ----
test_that('REQUIRE TEST for set with ls models including factors set within zelig call', {
    data(macro)
    z1 <- zelig(unem ~ gdp + trade + capmob + as.factor(country),
             model = "ls", data = macro)
    setUS1 <- setx(z1, country = "United States")

    z2 <- zelig(unem ~ gdp + trade + capmob + factor(country,
                                                    labels=letters[1:14]),
                model = "ls", data = macro)
    setUS2 <- setx(z2, country = "m")

    macro$country <- as.factor(macro$country)
    z3 <- zelig(unem ~ gdp + trade + capmob + country,
                model = "ls", data = macro)
    setUS3 <- setx(z3, country = "United States")

    expect_equal(setUS1$setx.out$x$mm[[1]][[16]], 1)
    expect_equal(setUS2$setx.out$x$mm[[1]][[16]], 1)
    expect_equal(setUS1$setx.out$x$mm[[1]][[16]],
                 setUS3$setx.out$x$mm[[1]][[16]])
    expect_equal(setUS2$setx.out$x$mm[[1]][[16]],
                 setUS3$setx.out$x$mm[[1]][[16]])
})

# REQUIRE TEST for set with ls models including natural logs set within zelig call --
test_that('REQUIRE TEST for set with ls models including natural logs set within zelig call', {
    z1 <- zelig(speed ~ log(dist), data = cars, model = 'ls')
    setd1 <- setx(z1, dist = log(15))

    cars$dist <- log(cars$dist)
    z2 <- zelig(speed ~ dist, data = cars, model = 'ls')
    setd2 <- setx(z2, dist = log(15))

    expect_equal(round(setd1$setx.out$x$mm[[1]][[2]], digits = 5), 2.70805)
    expect_equal(setd1$setx.out$x$mm[[1]][[2]],
               setd2$setx.out$x$mm[[1]][[2]])

    z3.1 <- zelig(Sepal.Length ~ log10(Petal.Length) + log(Sepal.Width),
              model = 'ls', data = iris, cite = FALSE)
    z3.2 <- zelig(Sepal.Length ~ log(Petal.Length, base = 10) +
                      log(Sepal.Width),
              model = 'ls', data = iris, cite = FALSE)
    expect_equal(unname(coef(z3.1)), unname(coef(z3.2)))

    setz3 <- setx(z3.1)
#    expect_equal(as.vector(round(unlist(setz3$setx.out$x), digits = 2)),
#                c(1, 1, 1.47, 1.12))
})

# REQUIRE TEST for ls with interactions ----------------------------------------
test_that('REQUIRE TEST for ls with interactions', {
    states <- as.data.frame(state.x77)
    z <- zelig(Murder ~ Income * Population, data = states, model = 'ls')
    s1 <- setx(z, Population = 1500:1600, Income = 3098)
    s2 <- setx(z, Population = 1500:1600, Income = 6315)

    expect_equal(length(s1$setx.out$range), 101)
    expect_equal(length(s2$setx.out$range), 101)
})

# REQUIRE TEST for ls with unrecognised variable name --------------------------
test_that('REQUIRE TEST for ls with unrecognised variable name', {
  states <- as.data.frame(state.x77)
  z <- zelig(Murder ~ Income * Population, data = states, model = 'ls')
  expect_error(setx(z, population = 1500:1600, Income = 3098),
               "Variable 'population' not in data set.")
})

# REQUIRE TEST for ls setrange with equal length ranges ------------------------
test_that('REQUIRE TEST for ls setrange with equal length ranges and polynomials', {
    iris.poly <- cbind(iris, I(iris$Petal.Length^2))
    names(iris.poly)[ncol(iris.poly)] <- 'pl_2'
    pl_range <- 1:7

    # Polynomial found outside of formula
    z.cars1 <- zelig(Sepal.Length ~ Petal.Length + pl_2 + Species,
                      data = iris.poly, model = 'ls', cite = FALSE)
    z.cars1 <- setx(z.cars1, Species = 'virginica', Petal.Length = pl_range,
                   pl_2 = pl_range^2)
    expect_equal(nrow(zelig_setx_to_df(z.cars1)), length(pl_range))

    # Polynomial found in formula
    z.cars2 <- zelig(Sepal.Length ~ Petal.Length + I(Petal.Length^2) + Species,
                      data = iris, model = 'ls', cite = FALSE)
    z.cars2 <- setx(z.cars2, Species = 'virginica', Petal.Length = pl_range)
    expect_equal(nrow(zelig_setx_to_df(z.cars2)), length(pl_range))
    expect_equal(zelig_setx_to_df(z.cars1)[[2]], zelig_setx_to_df(z.cars2)[[2]])
})

# REQUIRE TEST for . formulas --------------------------------------------------
test_that('REQUIRE TEST for . formulas', {
    z1 <- zelig(speed ~ ., data = cars, model = 'ls')
    zset <- setx(z1, dist = 5)
    expect_equal(names(coef(z1)), c("(Intercept)", "dist"))
})

# REQUIRE TEST for to_zelig within setx ----------------------------------------
test_that('REQUIRE TEST for to_zelig within setx', {
    m1 <- lm(speed ~ dist, data = cars)
    zset <- setx(m1, dist = 5)
    expect_equal(zset$setx.out$x$mm[[1]][2], 5)
    plot(sim(zset))

    m2 <- glm(speed ~ dist, data = cars, family = gaussian(link = "identity"))
    zset <- setx(m1, dist = 5)
    expect_equal(zset$setx.out$x$mm[[1]][2], 5)
    plot(sim(zset))
})
