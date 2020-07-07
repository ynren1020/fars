library(fars)
test_that("filename is a string of character with years", {
expect_equal(make_filename(2013), system.file("extdata", "accident_2013.csv.bz2", package = "fars"))
})
