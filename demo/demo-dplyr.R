library(Amelia)

data(freetrade)
df <- freetrade
imp <- amelia(x = df , cs = "country", m = 10) 

z5 <- zls$new()
z5
z5$zelig(gdp.pc ~ tariff + intresmi, data = imp)
z5
z5$zelig.out
z5$setx(tariff = 0.4)
z5$setx.out
# z5$setx1(tariff = 0.6)
z5$setx.out
z5$zelig.out
# z5$setrange(tariff = 1:3)
z5$setx.out
z5$zelig.out
z5
# TODO: Fix when re-running sim
z5$sim(10)
# z5$simx1(10)
z5$sim.out
# z5$zelig.out

z5 <- zls$new()
z5$zelig(gdp.pc ~ tariff + intresmi, data = dat, by = c("country", "year"))
z5$zelig.out
z5$setx(tariff = 0.4)
z5$setx1(tariff = 0.6)
z5$setrange(tariff = seq(0.2, 1.5, 0.1))
z5$zelig.out
z5$setx.out
.self <- z5
# TODO: Fix when re-running sim
z5$sim(10)
z5$zelig.out
z5$sim.out
z5$sim.out$range
