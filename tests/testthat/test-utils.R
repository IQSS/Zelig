context('test-utils.R')

test_that("REQUIRE TEST Median()", {
    input <- c(1, 2, 3)
    expected <- 2
    actual <- Median(input)
    expect_equal(actual, expected)
})

# REQUIRE TEST for to_zelig_mi -------------------------------------------------
test_that('REQUIRE TEST for to_zelig_mi', {
    set.seed(123)
    n <- 100
    x1 <- runif(n)
    x2 <- runif(n)
    y <- rnorm(n)
    data.1 <- data.frame(y = y, x = x1)
    data.2 <- data.frame(y = y, x = x2)

    mi.out <- to_zelig_mi(data.1, data.2)
    z.out.mi <- zelig(y ~ x, model = "ls", data = mi.out)

    expect_error(summary(z.out.mi), NA)
    expect_equivalent(round(as.numeric(z.out.mi$get_coef()[[1]][2]), 3), 0.1)
    expect_equivalent(round(as.numeric(combine_coef_se(z.out.mi)[[1]][1]), 3),
                            -0.122)

    z.out.mi.boot <- zelig(y ~ x, model = "ls", data = mi.out, bootstrap = 20)
    expect_equal(round(as.numeric(combine_coef_se(z.out.mi.boot)[[1]][1]), 3),
                    -0.094)

    expect_error(z.out.log <- zelig(y ~ log(x), model = "ls", data = mi.out),
                 NA)

    expect_error(z.out.log10 <- zelig(y ~ log(x, base = 10), model = "ls",
                                      data = mi.out), NA)
})

# REQUIRE TEST for combine_coef_se for bootstrapped ----------------------------
test_that('REQUIRE TEST for combine_coef_se for bootstrapped', {
    set.seed(123)
    n <- 100
    data.1 <- data.frame(y = rnorm(n), x = runif(n))
    z.out.boot <- zelig(y ~ x, model = "ls", data = data.1, bootstrap = 20)

    expect_error(summary(z.out.boot), NA)
    expect_equal(round(as.numeric(combine_coef_se(z.out.boot)[[1]][1]), 3),
                 0.007)
    summary(z.out.boot, bagging = TRUE)
    expect_equal(round(as.numeric(
                    combine_coef_se(z.out.boot, bagging = TRUE)[[1]][1]), 3),
                 -0.052)

    z5_ls <- zelig(Fertility ~ Education, model = "ls", data = swiss)
    expect_equal(length(combine_coef_se(z5_ls)), 3)
})

# REQUIRE TEST for to_zelig_mi -------------------------------------------------
test_that('REQUIRE TEST for to_zelig_mi -- with list of data.frames', {
    set.seed(123)
    n <- 100
    x1 <- runif(n)
    x2 <- runif(n)
    y <- rnorm(n)
    data.1 <- data.frame(y = y, x = x1)
    data.2 <- data.frame(y = y, x = x2)
    data_mi = list(data.1, data.2)

    mi.out <- to_zelig_mi(data_mi)
    z.out <- zelig(y ~ x, model = "ls", data = mi.out)

    expect_equivalent(round(as.numeric(z.out$get_coef()[[1]][2]), 3), 0.1)
})

# FAIL TEST for to_zelig_mi ----------------------------------------------------
test_that('FAIL TESTS for to_zelig_mi', {
    x <- 100
    expect_error(to_zelig_mi(x))
})

# FAIL TEST for or_summary -----------------------------------------------------
test_that("FAIL TEST for or_summary", {
    expect_error(or_summary(1:10), "obj must be of summary.glm class.")
})
