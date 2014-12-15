## Example 1

# Zelig 4 code:
library(Zelig4)
data(mid)
z.out1 <- Zelig4::zelig(conflict ~ major + contig + power + maxdem
                + mindem + years, data = mid, model = "relogit", tau = 1042 / 303772)
summary(z.out1)
x.out1 <- Zelig4::setx(z.out1)
set.seed(42)
s.out1 <- Zelig4::sim(z.out1, x = x.out1, num = 1000)
summary(s.out1)

# Zelig 5 code:
z5 <- zrelogit$new()
z5$zelig(conflict ~ major + contig + power + maxdem
         + mindem + years, data = mid, tau = 1042/303772)
z5
z5$setx()
z5$setx.out
set.seed(42)
z5$sim(num=1000)
z5$summarize()
z5$cite()
plot(z5)

z.out <- zelig(conflict ~ major + contig + power + maxdem + mindem + years, data = mid,
               tau = 1042/303772,
               model = "relogit")

## Example 2

# Zelig 4 code:
z.out2 <- zelig(conflict ~ major + contig + power + maxdem +
                  mindem + years, data = mid, model = "relogit", tau = 1042/303772,
                case.control = "weighting", robust = TRUE)
summary(z.out2)
x.out2 <- setx(z.out2)
set.seed(42)
s.out2 <- sim(z.out2, x = x.out2, num=1000)
summary(s.out2)

# Zelig 5 code:
z5 <- zrelogit$new()
z5$zelig(conflict ~ major + contig + power + maxdem +
           mindem + years, data = mid, tau = 1042/303772,
         case.control = "weighting")
z5
z5$setx()
set.seed(42)
z5$sim(num=1000)
z5$summarize()
z5$cite()

## Example 3: broken in Zelig 4

# # Zelig 4 code:
# 
# z.out2 <- zelig(conflict ~ major + contig + power + maxdem
#                     + mindem + years, data = mid, model = "relogit", tau = c(0.002, 0.005))
# summary(z.out2)
# x.out2 <- setx(z.out2)
# s.out <- sim(z.out2, x = x.out2)
# summary(s.out2)
# 
# 
# z.out3 <- zelig(conflict ~ major + contig + power + maxdem
#                     + mindem + years, data = mid, model = "relogit", tau = c(0.002, 0.005))
# summary(z.out3)
# x.out3 <- setx(z.out3)
# s.out3 <- sim(z.out3, x = x.out3)
# summary(s.out3)
# 
# # Zelig 5 code:
# z5 <- zrelogit$new()
# z5$zelig(conflict ~ major + contig + power + maxdem
#          + mindem + years, data = mid, tau = c(0.002, 0.005))
# z5
# z5$setx()
# set.seed(42)
# z5$sim(num=1000)
# z5$summarize()
# z5$cite()

# r <- relogit(conflict ~ major + contig + power + maxdem + mindem + years,
#              data = mid, tau = 1042/303772)
# rs
