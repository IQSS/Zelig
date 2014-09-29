# Zelig 4 code:
library(Zelig4)
data(mexico)
z.out <- Zelig4::zelig(vote88 ~ pristr + othcok + othsocok, model = "mlogit.bayes",
                       data = mexico)
summary(z.out)
x.out <- Zelig4::setx(z.out)
set.seed(42)
s.out <- Zelig4::sim(z.out, x = x.out, num = 1000)
summary(s.out)


# Zelig 5 code:
z5 <- zmlogitbayes$new()
z5$zelig(vote88 ~ pristr + othcok + othsocok,
         data = mexico, verbose = FALSE)
z5
z5$zelig.out
z5$setx()
set.seed(42)
z5$sim(num = 1000)
z5$summarize()
z5$cite()
