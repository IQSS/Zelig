# REQUIRE TEST toJSON ---------------------------------------------

test_that('REQUIRE TEST toJSON', {
	j <- createJSON(movefile=FALSE)
    	expect_true(j)
    	mypath <- file.path("zelig5models.json")
    expect_true(file.exists(mypath))
    expect_true(validate(readChar(mypath, file.info(mypath)$size)))
    file.remove(file.path(mypath))
})