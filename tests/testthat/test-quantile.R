# REQUIRE TEST quantile regression doc example ---------------------------------
test_that("REQUIRE TEST quantile regression doc example", {
    library(quantreg)
    library(dplyr)
    data("stackloss")

    z.out1 <- zelig(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.,
                    model = 'rq', data = stackloss)

    z.out2 <- zelig(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.,
                   model = 'rq', data = stackloss, tau = 0.5)
    z.set2 <- setx(z.out2, Air.Flow = seq(50, 80, by = 10))
    z.sim2 <- sim(z.set2)
    expect_error(plot(z.sim2), NA)

    z.out3 <- zelig(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.,
                    model = 'rq', data = stackloss, tau = 0.25)
    z.set3 <- setx(z.out3, Air.Flow = seq(50, 80, by = 10))
    z.sim3 <- sim(z.set3)
    expect_error(plot(z.sim3), NA)

    expect_equivalent(coef(z.out1)[[1]], coef(z.out2)[[1]])

    qr.out1 <- rq(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.,
                  data = stackloss, tau = 0.5)
    expect_equivalent(coef(z.out1)[[2]], coef(qr.out1)[[2]])

    expect_error(zelig(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.,
                    model = 'rq', data = stackloss, tau = c(0.25, 0.75)),
                 'tau argument only accepts 1 value.\nZelig is using only the first value.')
})

# REQUIRE TEST quantile regression with Amelia imputed data --------------------
test_that('REQUIRE TEST quantile regression with Amelia imputed data',{
    library(Amelia)
    library(dplyr)

    data(africa)
    a.out <- amelia(x = africa, cs = "country", ts = "year", logs = "gdp_pc")
    z.out <- zelig(gdp_pc ~ trade + civlib, model = "rq", data = a.out)

    expect_error(z.out %>% setx %>% sim %>% plot, NA)
})

