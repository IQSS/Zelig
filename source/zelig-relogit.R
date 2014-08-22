
## ----, eval = FALSE------------------------------------------------------
## z.out <- zelig(Y ~ X1 + X2, model = "relogit", tau = NULL,
##                        case.control = c("prior", "weighting"),
##                        bias.correct = TRUE, robust = FALSE,
##                        data = mydata, ...)
## x.out <- setx(z.out)
## s.out <- sim(z.out, x = x.out)


## ----, eval = FALSE------------------------------------------------------
## data(mid)


## ----, eval = FALSE------------------------------------------------------
## z.out1 <- zelig(conflict   major + contig + power + maxdem + mindem + years, + data = mid, model = “relogit”, tau = 1042/303772)


## ----, eval = FALSE------------------------------------------------------
## summary(z.out1)


## ----, eval = FALSE------------------------------------------------------
## x.out1 <- setx(z.out1)


## ----, eval = FALSE------------------------------------------------------
## s.out1 <- sim(z.out1, x = x.out1) RRR> summary(s.out1)


## ----, eval = FALSE------------------------------------------------------
## plot(s.out1)


## ----, eval = FALSE------------------------------------------------------
## z.out2 <- zelig(conflict   major + contig + power + maxdem + mindem
## + years, + data = mid, model = “relogit”, tau = 1042/303772, +
## case.control = “weighting”, robust = TRUE)


## ----, eval = FALSE------------------------------------------------------
## summary(z.out2)


## ----, eval = FALSE------------------------------------------------------
## x.out2 <- setx(z.out2)


## ----, eval = FALSE------------------------------------------------------
## s.out2 <- sim(z.out2, x = x.out2)
## summary(s.out2)


## ----, eval = FALSE------------------------------------------------------
## z.out2 <- zelig(conflict   major + contig + power + maxdem + mindem
## + + years, data = mid, model = “relogit”, + tau = c(0.002, 0.005))


## ----, eval = FALSE------------------------------------------------------
## summary(z.out2)


## ----, eval = FALSE------------------------------------------------------
## x.out2 <- setx(z.out2)


## ----, eval = FALSE------------------------------------------------------
## s.out <- sim(z.out2, x = x.out2)


## ----, eval = FALSE------------------------------------------------------
## summary(s.out2)


## ----, eval = FALSE------------------------------------------------------
## plot(s.out2)


