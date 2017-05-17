#### Integration tests for the Zelig estimate, set, sim, plot workflow      ####


# FAIL TEST sim workflow -------------------------------------------------------
test_that('FAIL TEST sim method warning if insufficient inputs', {
  z5 <- zls$new()
  expect_output(z5$zelig(Fertility ~ Education, model="ls", data = swiss),
                 'Argument model is only valid for the Zelig wrapper, but not the Zelig method, and will be ignored.')

  expect_warning(z5$sim(),
                 'No simulations drawn, likely due to insufficient inputs.')

  expect_error(z5$graph(), 'No simulated quantities of interest found.')
})

# FAIL TEST ci.plot range > length = 1 -----------------------------------------
test_that('FAIL TEST ci.plot range > length = 1', {
  z <- zls$new()
  z$zelig(Fertility ~ Education, data = swiss)
  expect_warning(z$setrange(Education = 5),
                 'Only one fitted observation provided to setrange.\nConsider using setx instead.')

  z$sim()
  expect_error(z$graph(),
               'Simulations for more than one fitted observation are required.')

  expect_warning(z$setrange1(Education = 5),
                 'Only one fitted observation provided to setrange.\nConsider using setx instead.')
  expect_error(z$graph(),
               'Simulations for more than one fitted observation are required.')
})

# REQUIRE TEST for by estimation workflow --------------------------------------
test_that('REQUIRE TEST for by estimation workflow', {
  # Majority Catholic dummy
  swiss$maj_catholic <- cut(swiss$Catholic, breaks = c(0, 51, 100))

  z5 <- zls$new()
  z5$zelig(Fertility ~ Education, data = swiss, by = 'maj_catholic')
  z5$setrange(Education = 5:15)
  z5$sim()

  expect_error(z5$graph(), NA)
})

# FAIL TEST for get_qi when applied to an object with no simulations ------------
test_that('FAIL TEST for get_qi when applied to an object with no simulations', {
    z <- zls$new()
    z$zelig(Fertility ~ Education, data = swiss)
    expect_error(z$get_qi(), 'No simulated quantities of interest found.')
})

# FAIL TEST for get_qi when unsupported qi supplied ----------------------------
test_that('FAIL TEST for get_qi when unsupported qi supplied', {
    z5 <- zls$new()
    z5$zelig(Fertility ~ Education, data = swiss)
    z5$setrange(Education = 5:15)
    z5$sim()
    expect_error(z5$get_qi(qi = "fa", xvalue = "range"), 'qi must be ev or pv.')
})

# FAIL TEST for estimation model failure ---------------------------------------
test_that('FAIL TEST for estimation model failure', {
  no_vary_df <- data.frame(y = rep(1, 10), x = rep(2, 10))
  z <- zarima$new()
  expect_error(z$zelig(y ~ x, data = no_vary_df),
               'Dependent variable does not vary for at least one of the cases.')
  expect_error(summary(z), 'Zelig model has not been estimated.')
})

# REQUIRE TEST for sim num argument --------------------------------------------
test_that('REQUIRE TEST for sim num argument', {
  z5 <- zls$new()
  z5$zelig(Fertility ~ Education, data = swiss)
  z5$setx(Education = 5)

  z5$sim()
  expect_equal(length(z5$get_qi()), 1000)

  z5$sim(num = 10) # Look into unexpected behaviour if sim order is reversed
  expect_equal(length(z5$get_qi()), 10)
})

# REQUIRE TEST from_zelig_model returns expected fitted model object -----------------
test_that('REQUIRE TEST from_zelig_model returns expected fitted model object', {
  z5 <- zls$new()
  z5$zelig(Fertility ~ Education, data = swiss)
  model_object <- z5$from_zelig_model()
  expect_is(model_object, class = 'lm')
  expect_equal(as.character(model_object$call[1]), 'lm')
})

# REQUIRE TEST from_zelig_model returns each fitted model object from mi -------------
test_that('REQUIRE TEST from_zelig_model returns each fitted model object from mi', {
  set.seed(123)
  n <- 100
  x1 <- runif(n)
  x2 <- runif(n)
  y <- rnorm(n)
  data.1 <- data.frame(y = y, x = x1)
  data.2 <- data.frame(y = y, x = x2)

  mi.out <- to_zelig_mi(data.1, data.2)
  z.out <- zelig(y ~ x, model = "ls", data = mi.out)
  model_list <- z.out$from_zelig_model()
  expect_is(model_list, class = 'list')
  expect_equal(as.character(model_list[[2]]$call[1]), 'lm')
})

# REQUIRE TEST functioning simparam with by and ATT ----------------------------
test_that('REQUIRE TEST functioning simparam with by and ATT', {
  set.seed(123)
  n <- 100
  xx <- rbinom(n = n, size = 1, prob = 0.3)
  zz <- runif(n)
  ss <- runif(n)
  rr <- rbinom(n, size = 1, prob = 0.5)
  mypi <- 1/(1 + exp(-xx -3*zz -0.5))
  yb <- rbinom(n, size = 1, prob = mypi)
  data <- data.frame(rr, ss, xx, zz, yb)

  zb.out <- zlogit$new()
  zb.out$zelig(yb ~ xx + zz, data = data, by = "rr")

  zb.out$ATT(treatment = "xx")
  out <- zb.out$get_qi(qi = "ATT", xvalue = "TE")
  expect_equal(length(out), 1000)
})

# REQUIRE TEST getters values and dimensions and plot does not fail-------------
test_that("REQUIRE TEST getters values and dimensions and plot does not fail",
{
    set.seed(123)
    n <- 1000
    myseq <- 1:n
    x <- myseq/n
    y <- x + (-1)^(myseq) * 0.1
    mydata <- data.frame(y = y, x = x)
    mydata2 <- data.frame(y = y, x = x + 2)
    z.out <- zelig(y ~ x, model = "ls", data = mydata)

    expect_equivalent(round(as.numeric(z.out$get_coef()[[1]]), 2), c(0, 1))
    expect_equivalent(length(z.out$get_predict()[[1]]), n)
    expect_equivalent(length(z.out$get_fitted()[[1]]), n)
    expect_equivalent(dim(z.out$get_vcov()[[1]]), c(2, 2))

    z.out$setx(x = 0)
    z.out$setx1(x = 1)
    show.setx <- summary(z.out)
    z.out$sim()
    show.sim <- summary(z.out)

    expect_equivalent(length(z.out$get_qi(qi = "ev", xvalue = "x")), n)
    expect_equivalent(round(mean(z.out$get_qi(qi = "ev", xvalue = "x")),
                            2), 0)
    expect_equivalent(length(z.out$get_qi(qi = "ev", xvalue = "x1")),
                      n)
    expect_equivalent(round(mean(z.out$get_qi(qi = "ev", xvalue = "x1")),
                            2), 1)

    expect_equivalent(length(z.out$get_qi(qi = "pv", xvalue = "x")), n)
    expect_equivalent(round(mean(z.out$get_qi(qi = "pv", xvalue = "x")),
                            2), 0)
    expect_equivalent(length(z.out$get_qi(qi = "pv", xvalue = "x1")),
                      n)
    expect_equivalent(round(mean(z.out$get_qi(qi = "pv", xvalue = "x1")),
                            2), 1)

    expect_equivalent(length(z.out$get_qi(qi = "fd", xvalue = "x1")),
                      n)
    expect_equivalent(round(mean(z.out$get_qi(qi = "fd", xvalue = "x1")), 2), 1)

    expect_false(show.setx[[1]])
    expect_false(show.sim[[1]])
    expect_true(is.null(plot(z.out)))

    xseq <- seq(from = 0, to = 1, length = 10)
    z.out$setrange(x = xseq)
    z.out$sim()

    expect_true(is.null(plot(z.out)))

    myref <- capture.output(z.out$references())
    expect_equivalent(substr(myref[1], 1, 11), "R Core Team")

    set.seed(123)
    boot.out <- zelig(y ~ x, model = "ls", bootstrap = 20, data = mydata)
    expect_equivalent(round(as.numeric(boot.out$get_coef()[[1]]), 2),
                      c(0, 1))

    show.boot <- summary(boot.out, bagging = TRUE)
    expect_false(show.boot[[1]])

    show.boot <- summary(boot.out, subset=2:3)
    expect_false(show.boot[[1]])


    set.seed(123)
    mi.out <- zelig(y ~ x, model = "ls", data = mi(mydata, mydata2))
    expect_equivalent(round(as.numeric(mi.out$get_coef()[[1]]), 2), c(0,
                                                                     1))
    expect_equivalent(round(as.numeric(mi.out$get_coef()[[2]]), 2), c(-2,
                                                                     1))
    expect_equivalent(length(mi.out$toJSON()), 1)

    show.mi <- summary(mi.out)
    expect_false(show.mi[[1]])
    show.mi.subset <- summary(mi.out, subset = 1)
    expect_false(show.mi.subset[[1]])
})

# REQUIRE TEST Binary QI's and ATT effects and BY argument-------------
test_that('REQUIRE TEST Binary QIs and ATT effects and BY argument', {
  set.seed(123)
  # Simulate data
  n <- 100
  xx <- rbinom(n = n, size = 1, prob = 0.5)
  zz <- runif(n)
  ss <- runif(n)
  rr <- rbinom(n, size = 1, prob = 0.5)
  mypi <- 1/ (1+exp(-xx -3*zz -0.5))
  yb <- rbinom(n, size = 1, prob = mypi)
  data <- data.frame(rr, ss, xx, zz, yb)

  # Estimate Zelig Logit models
  zb.out <- zlogit$new()
  zb.out$zelig(yb ~ xx + zz, data=data, by="rr")

  show.logit <- summary(zb.out)
  expect_false(show.logit[[1]])

  zb2.out <- zlogit$new()
  zb2.out$zelig(yb ~ xx, data=data)

  zb3.out <- zlogit$new()
  zb3.out$zelig(yb ~ xx + zz, data=data)

  x.high <- setx(zb.out, xx = quantile(data$xx, prob = 0.75))
  x.low <- setx(zb.out, xx = quantile(data$xx, prob = 0.25))
  s.out <- sim(zb.out, x = x.high, x1 = x.low)

  show.logit <- summary(s.out)
  expect_false(show.logit[[1]])
  expect_true(is.null(plot(s.out)))

  # Method to calculate ATT
  zb.out$ATT(treatment = "xx")

  # Getter to extract ATT
  out <- zb.out$get_qi(qi="ATT", xvalue="TE")
  expect_equal(length(out), 1000)

  # Plot ROC
  expect_true(is.null(rocplot(zb2.out, zb3.out)))
})

# REQUIRE TEST for get_names method----------------------------------------------
test_that('REQUIRE TEST for names field', {
  z <- zls$new()
  z$zelig(Fertility ~ Education, data = swiss)
  expect_is(z$get_names(), class = 'character')
  expect_false(is.null(names(z)))
})

# REQUIRE TEST for get_residuals method -----------------------------------------
test_that('REQUIRE TEST for get_residuals method', {
  z <- zls$new()
  z$zelig(Fertility ~ Education, data = swiss)
  expect_is(z$get_residuals(), class = 'list')
  expect_false(is.null(residuals(z)))
})

# REQUIRE TEST for get_df_residual method -----------------------------------------
test_that('REQUIRE TEST for get_df_residual method', {
  z <- zls$new()
  z$zelig(Fertility ~ Education, data = swiss)
  expect_equal(length(z$get_df_residual()), 1)
  expect_equal(length(df.residual(z)), 1)
})

# REQUIRE TEST for get_model_data method ---------------------------------------
test_that('REQUIRE TEST for get_model_data method', {
  z <- zls$new()
  z$zelig(Fertility ~ Education, data = swiss)
  expect_is(z$get_model_data(), class = 'data.frame')
})

# REQUIRE TEST for get_pvalue method ---------------------------------------
test_that('REQUIRE TEST for get_pvalue', {
  z <- zls$new()
  z$zelig(Fertility ~ Education, data = swiss)
  expect_is(z$get_pvalue()[[1]], class = 'numeric')
  expect_equal(z$get_pvalue()[[1]], get_pvalue(z)[[1]])
})

# REQUIRE TEST for get_se method ---------------------------------------
test_that('REQUIRE TEST for get_se', {
  z <- zls$new()
  z$zelig(Fertility ~ Education, data = swiss)
  expect_is(z$get_se()[[1]], class = 'numeric')
  expect_equal(z$get_se()[[1]], get_se(z)[[1]])
})

# REQUIRE TEST setx with logical covariates ------------------------------------
test_that('REQUIRE TEST setx with logical covariates', {
  swiss$maj_catholic <- cut(swiss$Catholic, breaks = c(0, 51, 100))
  swiss$maj_catholic_logical <- FALSE
  swiss$maj_catholic_logical[swiss$maj_catholic == '(51,100]'] <- TRUE
  z5l <- zls$new()
  z5l$zelig(Fertility ~ Education + maj_catholic_logical, data = swiss)
  z5l$setx(maj_catholic_logical = TRUE)
  expect_is(z5l$setx.out$x, class = c("rowwise_df", "tbl_df", "tbl",
                                        "data.frame"))
})

# REQUIRE TESTS for standard R methods with zelig models -----------------------
test_that('REQUIRE TESTS for standard R methods with zelig models', {
    z5 <- zls$new()
    z5$zelig(Fertility ~ Education, data = swiss)

    expect_equal(length(coefficients(z5)), length(coef(z5)), 2)
    expect_equal(nrow(vcov(z5)[[1]]), 2)
    expect_equal(length(fitted(z5)[[1]]), 47)
    expect_equal(length(predict(z5)[[1]]), 47)
})

