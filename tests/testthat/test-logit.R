library(testthat)

context("logit: Zelig 5 vs Zelig 4")

test_that("From Zelig 4 model vignette: 'Basic Example'", {
  
  seed <- 100
  nsim <- 10000
 
  library(Zelig)
  data(turnout)
  z.out <- Zelig::zelig(vote ~ age + race, model = "logit",
                                  data = turnout, cite = FALSE)
  x.out <- Zelig::setx(z.out, age = 36, race = "white")
  set.seed(seed)
  s.out <- Zelig::sim(z.out, x = x.out, num = nsim)
  
  z5 <- zlogit$new()
  z5$zelig(vote ~ age + race, data = turnout)
  z5$setx(age = 36, race = "white")
  set.seed(seed)
  z5$sim(nsim)
  
  # ev
  expect_equal(z5$sim.out$x[[1]]$ev, s.out$qi[[1]])
  # pv
  expect_equal(as.factor(z5$sim.out$x[[1]]$pv),
               factor(as.numeric(s.out$qi[[3]])))
  }
)

test_that("From Zelig 4 model vignette: 'Simulating First Differences'", {
  
  seed <- 100
  nsim <- 10
  
  library(Zelig)
  data(turnout)
  z.out <- Zelig::zelig(vote ~ race + educate, model = "logit",
                        data = turnout, cite = FALSE)
  x.high <- Zelig::setx(z.out, educate = quantile(turnout$educate, prob = 0.75))
  x.low <- Zelig::setx(z.out, educate = quantile(turnout$educate, prob = 0.25))
  set.seed(seed)
  s.out <- Zelig::sim(z.out, x = x.high, x1 = x.low, num = nsim)
  
  z5 <- zlogit$new()
  z5$zelig(vote ~ race + educate, data = turnout)
  z5$setx(educate = quantile(turnout$educate, prob = 0.75))
  z5$setx1(educate = quantile(turnout$educate, prob = 0.25))
  set.seed(seed)
  z5$sim(nsim)
  
  # ev
  expect_equal(z5$sim.out$x[[1]]$ev, s.out$qi[[1]])
  # pv
  expect_equal(as.factor(z5$sim.out$x[[1]]$pv),
               factor(as.numeric(s.out$qi[[3]])))
  # ev1
  expect_equal(z5$sim.out$x1[[1]]$ev, s.out$qi[[2]])
  # pv1
  expect_equal(as.factor(z5$sim.out$x1[[1]]$pv),
               factor(as.numeric(s.out$qi[[4]])))
  # fv
  expect_equal(z5$sim.out$fd[[1]], s.out$qi[[5]])
  }
)
