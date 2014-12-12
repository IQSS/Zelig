data(tobin)

z5 <- ztobitbayes$new()
z5$zelig(durable ~ age + quant, data = tobin, below = 1, above = 20, verbose = FALSE)
z5
z5$setx()
set.seed(42)
z5$sim()
z5$summarize()
