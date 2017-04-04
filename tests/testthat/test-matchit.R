# REQUIRE TEST for matched data using MatchIt ----------------------------------

#test_that('REQUIRE TEST for matched data using MatchIt', {
#    library(MatchIt)
#    library(optmatch)

#    data(lalonde)
#    m.out <- matchit(treat ~ educ + black + hispan + age, data = lalonde,
#                     method = "optimal")

#    z.out <- zelig(educ ~ treat + age, model = "ls", data = m.out)
#    s.out <- setx(z.out)

#    z.outl <- zelig(educ ~ treat + log(age), model = "ls", data = m.out)
#    s.outl <- setx(z.outl)

#    expect_false(s.out$setx.out$x$mm[[1]][3] == s.outl$setx.out$x$mm[[1]][3])
#})


# Not run due to unresolved environment issue
