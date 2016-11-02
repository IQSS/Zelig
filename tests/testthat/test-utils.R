context('test utils.R')

test_that("test Median()", {

	input <- c(1, 2, 3)
	expected <- 2
	actual <- Median(input)
	expect_equal(actual, expected)

})

