library(pscl)

source(file.path("..", "R", "utils.R"))
source(file.path("..", "R", "model-zelig.R"))
source(file.path("..", "R", "model-zip.R"))


data("bioChemists", package = "pscl")

## without inflation
## ("art ~ ." is "art ~ fem + mar + kid5 + phd + ment")
fm_pois <- glm(art ~ ., data = bioChemists, family = poisson)
fm_qpois <- glm(art ~ ., data = bioChemists, family = quasipoisson)
fm_nb <- glm.nb(art ~ ., data = bioChemists)

## with simple inflation (no regressors for zero component)
fm_zip <- zeroinfl(art ~ . | 1, data = bioChemists)
fm_zip2 <- zeroinfl(art ~ . | ., data = bioChemists)
summary(fm_zip2)

z5 <- zzip$new()
z5$zelig(art ~ phd + ment | ., data = bioChemists)
z5$zelig(art ~ . - phd - ment, data = bioChemists)
z5
.self <- z5
z5$setx(phd = 3, ment = 8)
z5$setx.out
set.seed(42)
z5$sim(num=100)
z5$summarize()

library(Zelig4)
z.out <- zelig(art ~ . - phd - ment, data = bioChemists, model = "ls")
summary(z.out)
x.out <- setx(z.out, phd = 3, ment = 8)
x.out
set.seed(42)
s.out <- sim(z.out, x.out, num = 100)
summary(s.out)

x.out <- setx(z.out)
x.out
set.seed(42)
s.out <- sim(z.out, x.out, num = 100)
s.out

# z5$sim()

library(Zelig4)
z.out <- zelig(art ~ phd * ment , bioChemists, model = "ls")
summary(z.out)
x.out <- setx(z.out)
x.out
set.seed(42)
s.out <- sim(z.out, x.out, num = 100)
summary(s.out)
