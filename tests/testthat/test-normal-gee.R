# REQUIRE TEST normal.gee with . formula ---------------------------------------
test_that('REQUIRE TEST normal.gee with . formula', {
    # test initially created by @andreashandel
    library(dplyr)

    # make some fake cluster ID
    mtcars$myid = sample(1:10, size = nrow(mtcars), replace = TRUE)

    # sort by cluster ID
    mydata <- mtcars %>% dplyr::arrange( myid)

    m1 <- geepack::geeglm(formula = mpg ~ ., family = gaussian, data = mydata,
                    id = mydata$myid) #this works

    z1 <- zelig(formula = mpg ~ ., model = "normal.gee", id = "myid",
                       data = mydata)

    expect_equal(coef(m1), coef(z1))
})
