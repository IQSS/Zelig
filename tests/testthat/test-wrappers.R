# Test missing model argument --------------------------------------------------
test_that('missing model argument error', {
    expect_error(zelig(Fertility ~ Education, data = swiss),
                 'Estimation model type not specified.\nSelect estimation model type with the model argument.')
})