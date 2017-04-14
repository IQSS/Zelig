# REQUIRE TEST quantile regression doc example ---------------------------------
test_that("REQUIRE TEST quantile regression doc example", {
    library(quantreg)
    data("stackloss")

    z.out1 <- zelig(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.,
                    model = 'rq', data = stackloss)

    z.out2 <- zelig(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.,
                   model = 'rq', data = stackloss, tau = 0.5)
    z.out2$setx()
    z.out2$sim()

    expect_equivalent(coef(z.out1)[[1]], coef(z.out2)[[1]])

    qr.out1 <- rq(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.,
                  data = stackloss, tau = 0.5)
    expect_equivalent(coef(z.out1)[[2]], coef(qr.out1)[[2]])

    z.out3 <- zelig(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.,
                    model = 'rq', data = stackloss, tau = c(0.25, 0.75))
    z.out3$setx()
    z.out3$sim()

    qr.out2 <- rq(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.,
                  data = stackloss, tau = c(0.25, 0.75))
    summary(qr.out2)
})
