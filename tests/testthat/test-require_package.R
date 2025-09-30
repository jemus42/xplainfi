test_that("require_package works", {
	expect_true(require_package("stats"))

	expect_error(require_package("foobar"), regexp = "Package .* required.*Install it from CRAN with")

	# With custom remote
	expect_error(
		require_package("foobar", from = "https://github.com/foo/bar"),
		regexp = "Package .* required.*Install it from .*https:\\/\\/github\\.com\\/foo\\/bar"
	)
})
