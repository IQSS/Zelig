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
    z.out <- zelig(y ~ x, model = "ls", data = mi.out)
    
    expect_equivalent(round(as.numeric(z.out$get_coef()[[1]][2]), 3), 0.1)
})

# FAIL TESTS for to_zelig_mi ----------------------------------------------------
test_that('FAIL TESTS for to_zelig_mi', {
    x <- 100
    expect_error(to_zelig_mi(x))
})
