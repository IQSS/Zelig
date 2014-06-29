library(testthat)

context("ls: Zelig 5 vs Zelig 4")

test_that("From Zelig 4 model vignette: 'Basic Example with First Differences'", {
  
  seed <- 100
  nsim <- 10000
 
  library(Zelig)
  data(macro)
  z.out <- Zelig::zelig(unem ~ gdp + capmob + trade,
                        model = "ls", data = macro, cite = FALSE)
  x.high <- Zelig::setx(z.out, trade = quantile(macro$trade, 0.8))
  x.low <- Zelig::setx(z.out, trade = quantile(macro$trade, 0.2))
  set.seed(seed)
  s.out <- Zelig::sim(z.out, x = x.high, x1 = x.low, num = nsim)
  
  z5 <- zls$new()
  z5$zelig(unem ~ gdp + capmob + trade, data = macro)
  z5$setx(trade = quantile(macro$trade, 0.8))
  z5$setx1(trade = quantile(macro$trade, 0.2))
  set.seed(seed)
  z5$sim(nsim)
  
  # ev
  expect_equal(z5$sim.out$x[[1]]$ev, s.out$qi[[1]])
  # pv
  expect_equal(z5$sim.out$x[[1]]$pv, s.out$qi[[3]])
  # ev1
  expect_equal(z5$sim.out$x1[[1]]$ev, s.out$qi[[2]])
  # pv1
  expect_equal(z5$sim.out$x1[[1]]$pv, s.out$qi[[4]])
  # fv
  expect_equal(z5$sim.out$fd[[1]], s.out$qi[[5]])
  }
)

test_that("From Zelig 4 model vignette: 'Using Dummy Variables'", {
  
  seed <- 100
  nsim <- 10
  
  library(Zelig)
  data(macro)
  z.out <- Zelig::zelig(unem ~ gdp + trade + capmob + as.factor(country),
                        model = "ls", data = macro, cite = FALSE)
  x.US <- Zelig::setx(z.out, country = "United States")
  x.Japan <- Zelig::setx(z.out, country = "Japan")
  set.seed(seed)
  s.out <- Zelig::sim(z.out, x = x.US, x1 = x.Japan, num = nsim)
  
  z5 <- zls$new()
  z5$zelig(unem ~ gdp + trade + capmob + as.factor(country), data = macro)
  z5$setx(country = "United States")
  z5$setx1(country = "Japan")
  set.seed(seed)
  z5$sim(nsim)
  
  # ev
  expect_equal(z5$sim.out$x[[1]]$ev, s.out$qi[[1]])
  # pv
  expect_equal(z5$sim.out$x[[1]]$pv, s.out$qi[[3]])
  # ev1
  expect_equal(z5$sim.out$x1[[1]]$ev, s.out$qi[[2]])
  # pv1
  expect_equal(z5$sim.out$x1[[1]]$pv, s.out$qi[[4]])
  # fv
  expect_equal(z5$sim.out$fd[[1]], s.out$qi[[5]])
}
)
