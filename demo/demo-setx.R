data(cars)

z5 <- zls$new()
z5$zelig(dist ~ speed, data = cars)
z5
z5$setx(speed = 30)
z5$setx.out$x

z5$setx1(speed = 40)
z5$setx.out$x1

z5$setx1(xssx = 0, jbcjdhsb = 987)
z5$setx.out$x1

z5$setx1(scs =98457, speed = 50, dcksj = 34)
z5$setx.out$x1

z5$sim(num=10)
z5$summarize()
