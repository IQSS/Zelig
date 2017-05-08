# REQUIRE TEST normal.gee with . formula ---------------------------------------
test_that('REQUIRE TEST normal.gee with . formula', {
    # test initially created by @andreashandel
    library(dplyr)

    # make some fake cluster ID
    mtcars$myid = sample(1:10, size = nrow(mtcars), replace = TRUE)

    # sort by cluster ID
    mydata <- mtcars %>% dplyr::arrange(myid)

    m1 <- geepack::geeglm(formula = mpg ~ ., family = gaussian, data = mydata,
                    id = mydata$myid) #this works

    z1 <- zelig(formula = mpg ~ ., model = "normal.gee", id = "myid",
                       data = mydata)

    expect_equal(coef(m1), coef(z1))

    z.set <- setx(z1)
    z.sim <- sim(z.set)

    expect_equal(nrow(zelig_qi_to_df(z.sim)), 1000)
})

# REQUIRE TEST normal.gee with multiply imputed data ---------------------------
test_that('REQUIRE TEST normal.gee with . formula', {
    # test initially created by @andreashandel
    library(dplyr)

    # make some fake cluster ID
    mtcars$myid = sample(1:10, size = nrow(mtcars), replace = TRUE)

    # sort by cluster ID
    mydata1 <- mtcars %>% dplyr::arrange(myid) %>% as.data.frame
    mydata2 = mydata1

    # create MI data
    mydata_mi <- to_zelig_mi(mydata1, mydata2)

    zmi <- zelig(formula = mpg ~ cyl + disp, model = "normal.gee", id = "myid",
                data = mydata_mi)

    expect_error(summary(zmi), NA)

    z.set <- setx(zmi)
    z.sim <- sim(z.set)

    expect_equal(nrow(zelig_qi_to_df(z.sim)), 1000)
})

