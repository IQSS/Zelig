
## ----, eval = FALSE------------------------------------------------------
## z5 <- zrelogit$new()
## z5$zelig(Y ~ X1 + X2, tau = NULL,
##                        case.control = c("prior", "weighting"),
##                        bias.correct = TRUE, robust = FALSE,
##                        data = mydata, ...)
## z5$setx()
## z5$sim()


## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(Y ~ X1 + X2, model = "relogit", tau = NULL,
##                        case.control = c("prior", "weighting"),
##                        bias.correct = TRUE, robust = FALSE,
##                        data = mydata, ...)
## x.out <- setx(z.out)
## s.out <- sim(z.out, x = x.out)


## ----, eval = TRUE, echo = FALSE-----------------------------------------
suppressWarnings(suppressMessages(library(Zelig)))


## ----, eval = TRUE-------------------------------------------------------
data(mid)


## ----, eval = TRUE-------------------------------------------------------
z.out1 <- zelig(conflict ~ major + contig + power + maxdem + mindem + years, data = mid, model = "relogit", tau = 1042/303772)


## ----, eval = TRUE-------------------------------------------------------
summary(z.out1)


## ----, eval = TRUE-------------------------------------------------------
x.out1 <- setx(z.out1)


## ----, eval = TRUE-------------------------------------------------------
s.out1 <- sim(z.out1, x = x.out1)
summary(s.out1)


## ----Zelig-relogit, dev=c("png", "pdf"), eval = TRUE, fig.cap = "Zelig-relogit"----
plot(s.out1)


## ----, eval = TRUE-------------------------------------------------------
z.out2 <- zelig(conflict ~ major + contig + power + maxdem + mindem + years, data = mid, model = "relogit", tau = 1042/303772, case.control = "weighting", robust = TRUE)


## ----, eval = TRUE-------------------------------------------------------
summary(z.out2)


## ----, eval = TRUE-------------------------------------------------------
x.out2 <- setx(z.out2)


## ----, eval = TRUE-------------------------------------------------------
s.out2 <- sim(z.out2, x = x.out2)
summary(s.out2)


## ----, eval = TRUE-------------------------------------------------------
z.out2 <- zelig(conflict ~ major + contig + power + maxdem + mindem + years, data = mid, model = "relogit", tau = c(0.002, 0.005))


## ----, eval = TRUE-------------------------------------------------------
z.out2


## ----, eval = TRUE-------------------------------------------------------
x.out2 <- setx(z.out2)


## ----, eval = TRUE-------------------------------------------------------
s.out <- sim(z.out2, x = x.out2)


## ----, eval = TRUE-------------------------------------------------------
summary(s.out2)


## ----, dev=c("png", "pdf"), eval = TRUE, fig.cap = ""--------------------
plot(s.out2)


