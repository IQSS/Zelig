# REQUIRE TEST weighting ---------------------------------------------

test_that('REQUIRE TEST weighting', {

	set.seed(123)
	x <- runif(90)
	y <- c( 2*x[1:45], -3*x[46:90] ) + rnorm(90)
	z <- as.numeric(y>0)
	w1 <- c(rep(1.8, 45), rep(0.2,45))
	mydata <- data.frame(z,y,x,w1)

	w2 <- rep(c(1.8,0.2), 45)

	z1.out <- zelig( y ~ x, cite = FALSE, model = "ls", weights = "w1",
	                 data = mydata)
	expect_equivalent(length(z1.out$get_coef()[[1]]),2)

	z2.out <- zelig( y ~ x, cite=FALSE, model="ls", weights=w2, data=mydata)
	expect_equivalent(length(z2.out$get_coef()[[1]]),2)

	z3.out <- zls$new()
	expect_warning(z3.out$zelig( y ~ x, weights="noSuchName", data=mydata))

	z4.out <- zls$new()
	expect_warning(z4.out$zelig( y ~ x, weights=w2[1:10], data=mydata))

	continuous.weights <- rep(x=c(0.6, 1, 1.4), times=30)
	z5.out <- zelig( z ~ x, model="logit", weights=continuous.weights, data=mydata)
	expect_equivalent(length(z5.out$get_coef()[[1]]),2)

	integer.weights <- rep(x=c(0, 1, 2), times=30)
	z6.out <- zelig( z ~ x, model="logit", weights=integer.weights, data=mydata)
	expect_equivalent(length(z6.out$get_coef()[[1]]),2)

})
