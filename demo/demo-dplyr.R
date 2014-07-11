library(dplyr)
library(plyr)
library(Amelia)
library(MASS)
# library(Zelig5)

data(freetrade)
df <- freetrade
imp <- amelia(x = df , cs = "country", m = 10) 
idata <- imp$imputations
MI <- length(idata)
iidata <- lapply(seq(MI), function(mi) cbind(mi, idata[[mi]]))
ddata <- rbind_all(iidata)

ddata <- na.omit(ddata)

dat <- ddata
# dat <- tbl_df(ddata)
# ID <- c("country")

z5 <- zls$new()
z5$zelig(gdp.pc ~  tariff + intresmi, data = dat)
z5$zelig.out
z5$set()
z5$setx(tariff = 0.4)
z5$setx.out
z5$setx1(tariff = 0.6)
z5$setx.out
z5$zelig.out
z5$setrange(tariff = 1:3)
z5$setx.out
z5$zelig.out
# TODO: Fox when re-running sim
z5$sim(10)
# z5$simx1(10)
z5$sim.out
z5$zelig.out

z5 <- zls$new()
z5$zelig(gdp.pc ~  tariff + intresmi, data = dat, by = "country")
z5$zelig.out
z5$setx(tariff = 0.4)
z5$setx1(tariff = 0.6)
z5$setrange(tariff = seq(0.2, 1.5, 0.1))
z5$zelig.out
z5$setx.out
# TODO: Fox when re-running sim
z5$sim(10)
z5$zelig.out
z5$sim.out
z5$sim.out$range

d <- tbl_df(do.call("cbind", z5$sim.out$range))
d

i <- filter(z5$zelig.out, country %in% c("India", "Nepal"))
i
i$ev
i$pv

z5 <- zls$new()
z5$zelig(gdp.pc ~  tariff + intresmi, data = dat, by = c("mi", "year", "country"))
z5$zelig.out

z5 <- zls$new()
z5$zelig(gdp.pc ~  tariff, data = dat, by = c("mi", "year", "intresmi", "country"))
z5$zelig.out

z5 <- zls$new()
z5$zelig(gdp.pc ~  tariff + intresmi, data = dat, by = c("country"))
z5$zelig.out
z5$setx(tariff = 0.4)
z5$zelig.out
z5$zelig.out
z5$sim(10)

.self <- z5
ID = "ID"


summarise_each(d, funs(qis = .self$qi(.$simparam, .), starts_with("mmr")))

d %>%
  do(qis = .self$qi(.$simparam, .$mmr1)) %>%
  do(evr1 = .$qis$ev, pvr1 = .$qis$pv)

d <- inner_join(.self$zelig.out, .self$setx.out)

Filter(function(x) names(x) )

d %>%
  do(function(x){
    sapply(Filter(is.numeric, x), mean)  
  })

mutate_each(funs(half = list(.)))

d[["mmr1
   "]]

.self$qi(d$simparam[[1]], d$mmr1[[1]])
.self$qi(d$simparam[[1]], d$mmr1[[1]])
.self$qi(d$simparam[[1]], d$mmr1[[1]])

z5$num <- 10
z5$param()

s.out <- z5$set()
filter(s.out, country == "Nepal")
z5$setx(tariff = 0.4)
# z5$setx.out
# z5$setx1(usheg = 0.4)
z5$setx.out
z5$sim(num = 5)
z5$sim.out

z5$setrange(usheg = c(0.4, 0.6))

z5$param()
z5$simparam
z5$simparam$simparam[[1]]
z5$qi()
z5$qi()$ev[[1]]

require(dplyr)

fn <- function(formula, data) {
  zc <- match.call(expand.dots=TRUE)
  mc <- match.call(expand.dots=TRUE)
  mc[[1]] <- quote(stats::lm)
  return(mc)
}

fn2 <- function(fc, data) {
  fc$data <- data
  return(fc)
}

data(cars)

fc <- fn(dist ~ speed, cars)
fn2(fc, .)

require(Zelig)
data(turnout)

lm(vote ~ age, data = turnout)
fc <- fn(vote ~ age, data = turnout)
fc

ID <- "race"


z.out <- turnout %>% 
  regroup(lapply(ID, as.symbol)) %>% 
  do(r = eval(fn2(fc, quote(.))))

z.out$r

# df.h <- group_by(ddata, year, country)

fn <- quote(stats::lm)
f <- gdp.pc ~  tariff
f1 <- 1 ~  tariff
num <- 1000

s <- list(polity = 10)
# http://stackoverflow.com/questions/21390141/specify-dplyr-column-names
library(Zelig)
dat <- data(turnout)
f <- vote ~ age + income
f1 <- 1 ~ age + income
s <- list(income = 3)
num <- 10
fn <- quote(lm)
library(Zelig)
dat <- turnout
#1
ID = "race"
#2
ID = NULL
ID = 'ID'
dat <- cbind(1 , dat)
names(dat)[1] <- "ID"
head(dat)

ddply(dat, ID, eval(fn)(f, data = .))

zelig.out <- dat %>% 
  regroup(lapply(ID, as.symbol)) %>% 
  do(model = eval(fn)(f, data = .))

zelig.out

setx.out <- dat %>% 
  regroup(lapply(ID, as.symbol)) %>% 
  do(setx = reduce(dataset = ., s))

setx.out
setx.out$setx

update <- setx.out %>%
  do(mm = model.matrix(f1, .$setx))

update

update$mm[[1]]

param <- zelig.out %>%
  do(simparam = mvrnorm((num), coef(.$model), vcov(.$model)))

z.out <- mutate(zelig.out, setx = setx.out$setx,
                mm = update$mm, simparam = param$simparam)

s.out <- z.out %>%
  do(s.out = .$simparam %*% t(.$mm))

z.out <- mutate(zelig.out, setx = setx.out$setx,
                mm = update$mm, simparam = param$simparam, s.out = s.out$s.out)
z.out

sim.out <- lapply(1:nrow(zelig.out),
             function(i) 
               param$simparam[i][[1]] %*% t(update$mm[[i]]))

# pe <- ev

# qi <- lapply(ev, Zelig5::statmat)

reduce <- function(dataset, s) {
  dataset <- as.data.frame(dataset)
  ldata <- lapply(dataset, avg)
  if (length(s) > 0) {
    pred <- terms(zelig.out$model[[1]], "predvars")
    n <- union(as.character(attr(pred, "predvars"))[-1],
               names(dataset))
    if (is.list(s[[1]]))
      s <- s[[1]]
    m <- match(names(s), n)
    ma <- m[!is.na(m)]
    if (!all(complete.cases(m))) {
      w <- paste("Variable '", names(s[is.na(m)]),
                 "' not in data set.\n", sep = "")
      warning(w)
    }
    for (i in seq(n[ma]))
      ldata[n[ma]][i][[1]] <- setval(dataset[n[ma]][i][[1]],
                                     s[n[ma]][i][[1]])
  }
  return(ldata)
}


by_cyl <- group_by(mtcars, cyl)
do(by_cyl, head(., 2))

models <- by_cyl %>% do(mod = lm(mpg ~ disp, data = .))
models

summarise(models, rsq = summary(mod)$r.squared)
models %>% do(data.frame(coef = coef(.$mod)))
models %>% do(data.frame(
  var = names(coef(.$mod)),
  coef(summary(.$mod)))
)

models <- by_cyl %>% do(
  mod_linear = lm(mpg ~ disp, data = .),
  mod_quad = lm(mpg ~ poly(disp, 2), data = .)
)
models
compare <- models %>% do(aov = anova(.$mod_linear, .$mod_quad))


mod <- list(order=1:5, x=11:15)
dat <- data.frame(a=letters[1:5], b=letters[11:15])

foo <- function(model, data) {
  data %>% mutate(x=model$x, o=model$order)
}

bar <- function(model, data) {
  ord <- model$order
  data %>% mutate(x=model$x, o=ord)
}

# this works 
bar(mod,dat)

# this fails with "invalid subscript type 'closure'"
foo(mod, dat)
