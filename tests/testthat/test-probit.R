library(testthat)

context("Unit Testing Zelig-probit (Probit Regression)")

test_that("Monte Carlo Testing Zelig-probit", {
  
  z <- zprobit$new()
  set.seed(93544)
  test <- z$test()
  
  expect_that(test$kstest$p.value > .05, is_true() )
}
)


