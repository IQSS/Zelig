# REQUIRE TEST quantile regression doc example ---------------------------------
test_that("REQUIRE TEST quantile regression doc example", {
    data("stackloss")

    z.out1 <- zelig(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.,
                    model = 'rq', data = stackloss)

    z.out2 <- zelig(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.,
                   model = 'rq', data = stackloss, tau = 0.5)

    expect_equivalent(coef(z.out1)[[1]], coef(z.out2)[[1]])

#    z.out3 <- zelig(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.,
#                   model = 'rq', data = stackloss, tau = c(0.25, 0.75))
})
