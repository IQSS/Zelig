data(macro)
head(macro)

z5 <- zls$new()
z5$zelig(unem ~ gdp + trade + capmob + country, data = macro)
z5
z.out <- z5$zelig.out$z.out[[1]]
z5$setx(gdp = 6, country = "Japan")
z5$setx(gdp = 5)
z5$setx.out$x$mm[[1]]

##----- "by" imputation

data("freetrade", package = "Amelia")
imp <- amelia(x = freetrade , cs = "country", m = 10) 
z5 <- zls$new()
z5
z5$zelig(gdp.pc ~ tariff + intresmi, data = imp)
z5
z5$zelig.out
z5$setx()

##----- "by"

data(turnout)
z5 <- zls$new()
z5$zelig(vote ~ age , data = turnout, by = c("race"))
z5$zelig.out
z5
z5$setx(age = 40)
z5$setx.out$x$mm
