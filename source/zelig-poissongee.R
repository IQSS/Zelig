
## ----, eval = FALSE------------------------------------------------------
## z5 <- zgammagee$new()
## z5$zelig(Y ~ X1 + X2, model = "poisson.gee",
##          id = "X3", data = mydata)
## z5$setx()
## z5$sim()


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(Y ~ X1 + X2, model = "poisson.gee",
##                id = "X3", data = mydata)
## x.out <- setx(z.out)
## s.out <- sim(z.out, x = x.out)


## ----, eval = TRUE, echo = FALSE-----------------------------------------
suppressWarnings(suppressMessages(library(Zelig)))


## ----, eval = TRUE-------------------------------------------------------
data(sanction)


## ----, eval = TRUE-------------------------------------------------------
sanction$cluster <- c(rep(c(1:15), 5), rep(c(16), 3))


## ----, eval = TRUE-------------------------------------------------------
sorted.sanction <- sanction[order(sanction$cluster), ]


## ----, eval = TRUE-------------------------------------------------------
z.out <- zelig(num ~ target + coop, model = "poisson.gee",
               id = "cluster", data = sorted.sanction)
summary(z.out)


## ----, eval = TRUE-------------------------------------------------------
x.out <- setx(z.out)


## ----, eval = TRUE-------------------------------------------------------
s.out <- sim(z.out, x = x.out)
summary(s.out)


## ----Zelig-poissongee, dev=c("png", "pdf"), eval = TRUE, fig.cap = "Zelig-poisson"----
plot(s.out)


