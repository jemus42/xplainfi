test_that("settin goptions works", {
	# both are consistent if unset -> FALSE
	Sys.unsetenv("XPLAIN_FOO")
	options("xplain.foo" = NULL)

	expect_false(xplain_opt("foo"))

	# option has precedence over env
	Sys.setenv("XPLAIN_FOO" = TRUE)
	options("xplain.foo" = FALSE)

	expect_false(xplain_opt("foo"))

	# option has precedence over env
	Sys.setenv("XPLAIN_FOO" = FALSE)
	options("xplain.foo" = TRUE)

	expect_true(xplain_opt("foo"))

	# both are consistent
	Sys.setenv("XPLAIN_FOO" = FALSE)
	options("xplain.foo" = FALSE)

	expect_false(xplain_opt("foo"))
})
