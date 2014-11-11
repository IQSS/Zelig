# library(testthat)
# 
# context("lognorm: Zelig 5 vs Zelig 4")
# 
# test_that("From Zelig 4 model vignette: 'Example'", {
#   
#   seed <- 100
#   nsim <- 10000
#  
#   library(Zelig)
#   data(coalition)
#   z.out <- Zelig::zelig(Surv(duration, ciep12) ~ fract + numst2, model = "lognorm",
#                 data = coalition, cite = FALSE)
#   x.low <- Zelig::setx(z.out, numst2 = 0)
#   x.high <- Zelig::setx(z.out, numst2 = 1)
#   set.seed(seed)
#   s.out <- Zelig::sim(z.out, x = x.low, x1 = x.high, num = nsim)
#   
#   z5 <- zlognorm$new()
#   z5$zelig(Surv(duration, ciep12) ~ fract + numst2, data = coalition)
#   z5$setx(numst2 = 0)
#   z5$setx1(numst2 = 1)
#   set.seed(seed)
#   z5$sim(nsim)
#   
#   # ev
#   expect_equal(z5$sim.out$x[[1]]$ev, s.out$qi[[1]])
#   # pv
#   expect_equal(z5$sim.out$x[[1]]$pv, s.out$qi[[3]])
#   # ev1
#   expect_equal(z5$sim.out$x1[[1]]$ev, s.out$qi[[2]])
#   # pv1
#   expect_equal(z5$sim.out$x1[[1]]$pv, s.out$qi[[4]])
#   # fv
#   expect_equal(z5$sim.out$fd[[1]], s.out$qi[[5]])
#   }
# )
