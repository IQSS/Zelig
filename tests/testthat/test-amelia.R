# REQUIRE TEST for Amelia integration, no-transformations ----------------------

test_that('REQUIRE TEST for Amelia integration, no-transformations', {
    library(Amelia)

    data(africa)
    a.out <- amelia(x = africa, cs = "country", ts = "year", logs = "gdp_pc")
    z.out <- zelig(gdp_pc ~ trade + civlib, model = "ls", data = a.out)
    z.set <- setx(z.out)
    z.sim <- sim(z.set)
    expect_equal(mean(z.sim$get_qi()), 1000, tolerance = 100)
})


test_that('REQUIRE TEST for Amelia integration, log-transformation', {
    library(Amelia)

    data(africa)
    a.out <- amelia(x = africa, cs = "country", ts = "year", logs = "gdp_pc")
    z.out <- zelig(gdp_pc ~ trade + civlib, model = "ls", data = a.out)
    z.outl <- zelig(gdp_pc ~ log(trade) + civlib, model = "ls", data = a.out)
    expect_false(coef(z.out)[[1]][2] == coef(z.outl)[[1]][2])
})
