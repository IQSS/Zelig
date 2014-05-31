library(Zelig)
data(macro)

source(file.path("..", "R", "utils.R"))
source(file.path("..", "R", "model-zelig.R"))
source(file.path("..", "R", "model-ls.R"))

## Zelig 4
z.out <- zelig(unem ~ gdp + trade + capmob +
                 country,
               model = "ls", data = macro)
summary(z.out)

x.US <- setx(z.out, country = "United States")
x.US
x.Japan <- setx(z.out, country = "Japan")
x.Japan

s.out <- sim(z.out, x = x.US, x1 = x.Japan)
summary(s.out)

## Zelig 5
z5 <- zls$new()
z5$zelig(unem ~ gdp + trade + capmob +
           country, data = macro)
z5
z5$setx(country = "United States")
z5$setx.out

z5$setx1(country = "Japan")
z5$setx.out$x1

z5$sim()
z5$summarize()
