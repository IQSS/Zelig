# Zelig 4 code:
library(Zelig4)
data(sanction)
sanction$ncost <- factor(sanction$ncost,
                         ordered = TRUE,
                         levels = c("net gain",
                                    "little effect", "modest loss", "major loss"))
z.out <- Zelig4:: zelig(ncost ~ mil + coop, model = "oprobit.bayes", data = sanction,
                        verbose = TRUE)

summary(z.out)
x.out <- Zelig4::setx(z.out)
set.seed(42)
s.out <- Zelig4::sim(z.out, x = x.out, num = 1000)
summary(s.out)

# Zelig 5 code:
z5 <- zoprobitbayes$new()
z5$zelig(ncost ~ mil + coop, data = sanction, verbose = FALSE)
z5
z5$zelig.out
z5$setx()
set.seed(42)
z5$sim(num = 1000)
z5$summarize()
z5$cite()
