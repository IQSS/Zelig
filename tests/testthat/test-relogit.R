# REQUIRE TEST Monte Carlo test relogit ------------------------------------------

test_that('REQUIRE TEST relogit Monte Carlo', {
    z <- zrelogit$new()
    test.relogit <- z$mcunit(alpha=0.1, b0=-4, nsim=1000)
    expect_true(test.relogit)
})
