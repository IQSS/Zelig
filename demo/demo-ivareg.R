data("CigarettesSW", package = "AER")
CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)
## model
fm <- ivreg(log(packs) ~ log(rprice) + log(rincome) |
              log(rincome) + tdiff + I(tax/cpi),
            data = CigarettesSW, subset = year == "1995")
summary(fm)
summary(fm, vcov = sandwich, df = Inf, diagnostics = TRUE)

# Zelig 5 code:
z5 <- zivareg$new()
z5$zelig(log(packs) ~ log(rprice) + log(rincome) |
           log(rincome) + tdiff + I(tax/cpi), data = CigarettesSW, subset = year == "1995")
summary(z5$zelig.out$z.out[[1]])

z5$zelig(log(packs) ~ log(rprice) + log(rincome) |
           log(rincome) + tdiff + I(tax/cpi), data = CigarettesSW)
summary(z5$zelig.out$z.out[[1]])


z5$model.call
z5$zelig.call
z5$set()
z5$setx(rprice = 3)
z5$setx.out
z5$setx.out$x$mm[[1]]
set.seed(42)
z5$sim(num=3)
z5$sim.out
z5$sim.out$x$ev[[1]]
z5$sim.out$x$pv[[1]]
# .self <- z5
# z5$ev
# z5$ev("x")
z5$summarize()
z5$cite()