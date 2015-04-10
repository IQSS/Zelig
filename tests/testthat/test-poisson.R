# library(testthat)
# 
# context("Unit Testing Zelig-poisson (Poisson Regression)")
# 
# test_that("Monte Carlo Testing Zelig-poisson", {
#   
#   z <- zpoisson$new()
#   set.seed(93544)
#   test <- z$test()
#   
#   expect_that(test$kstest$p.value > .05, is_true() )
# }
# )