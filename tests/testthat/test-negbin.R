# library(testthat)
# 
# context("Unit Testing Zelig-negbin (Negative Binomial Regression)")
# 
# test_that("Monte Carlo Testing Zelig-negbin", {
#   
#   z5 <- znegbin$new()
#   set.seed(935441)
#   test <- z5$test()
#   
#   expect_that(test$kstest$p.value > .05, is_true() )
# }
# )