library(Zelig3)

data(coalition)
z.out1 <- Zelig3::zelig(Surv(duration, ciep12) ~ numst2  + strata(polar),
                        model = "coxph", data = coalition, by = "invest")
summary(z.out1)
x.out1 <- Zelig3::setx(z.out1)
x.out1
s.out1 <- Zelig3::sim(z.out1, x.out1)
summary(s.out1)
plot(s.out1)


hmohiv <-read.table("http://www.ats.ucla.edu/stat/r/examples/asa/hmohiv.csv", sep=",", header = TRUE)

z5 <- zcoxph$new()
z5
z5$zelig(Surv(time,censor) ~ drug, method = "breslow", data = hmohiv, model = FALSE)
z5
z.out <- z5$zelig.out$z.out[[1]]
z5$setx()
z5$setx.out
z5$setx.out$x$mm
z5$sim()

set.seed(1234)
z5 <- zcoxph$new() 
z5
z5$zelig(Surv(duration, ciep12) ~ invest + numst2 + crisis, model = "coxph", data = head(coalition, 31), x = TRUE)
z5
z5$setx()
z5$sim()
z5


zs <- zcoxph$new() 
zs
zs$zelig(Surv(duration, ciep12) ~ numst2 + crisis, model = "coxph", data = coalition, by = "invest")
zs
zs$setx()
zs$setx.out$x
zs$sim()
zs


statmat(z5$sim.out$x$ev[[1]])
statmat(z5$sim.out$x$cumhaz[[1]])
t(z5$sim.out$x$hazard[[1]])

statmat(z5$sim.out$x$survival[[1]])
statmat(z5$sim.out$x$cumhaz[[1]])
t(z5$sim.out$x$hazard[[1]])


set.seed(1234)
z5 <- zcoxph$new() 
z5
z5$zelig(Surv(duration, ciep12) ~ invest + numst2 + crisis, model = "coxph",
         data = head(coalition, 31), x = TRUE)
z5
z5$setx()
z5$sim()
z5

set.seed(1234)
z.out1 <- Zelig3::zelig(Surv(duration, ciep12) ~ invest + numst2 + crisis,
                        model = "coxph", data = coalition)
summary(z.out1)
x.out1 <- Zelig3::setx(z.out1)
x.out1
s.out1 <- Zelig3::sim(z.out1, x.out1)
summary(s.out1)

plot(s.out1)

z5 <- zcoxph$new()
z5
z5$zelig(Surv(duration, ciep12) ~ invest +  strata(crisis) + strata(numst2) + polar, model = "coxph", data = coalition, x = TRUE)
z5$zelig.out$z.out[[1]]$x
.self <- z5

z5$setx()
z5$setx.out$x$mm[[1]]
z5$sim(10)
z5
sf <- survfit(z5$zelig.out$z.out[[1]])
sf

.self <- z5
reduce(dataset = .self$data, s = NULL, formula = .self$formula, data = .self$data)

z5$set()
z5$sim()

survival::coxph(formula = Surv(duration, ciep12) ~  crisis + strata(polar) + strata(invest), model = FALSE, data = coalition)

z5 <- zcoxph$new()
z5
z5$zelig(Surv(duration, ciep12) ~ crisis + strata(invest), model = "coxph", data = coalition, x = TRUE)
z5$zelig.out$z.out[[1]]$x
.self <- z5
z5$setx()
z5$sim(10)
z5

test1 <- list(time=c(4,3,1,1,2,2,3), 
              status=c(1,1,1,0,1,1,0), 
              x=c(0,2,1,1,1,0,0), 
              sex=c(0,0,0,0,1,1,1)) 
# Fit a stratified model 
coxph() 


drug.coxph <- coxph()
summary(drug.coxph)

data(coalition)
z.out1 <- Zelig3::zelig(Surv(duration, ciep12) ~ invest +  strata(crisis) + numst2,
                        model = "coxph", data = coalition)
summary(z.out1)
x.out1 <- Zelig3::setx(z.out1)
x.out1
s.out1 <- Zelig3::sim(z.out1, x.out1)
summary(s.out1)
plot(s.out1)
