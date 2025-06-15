test_that("bibentries are valid", {
  expect_s3_class(bibentries, "bibentry")
  expect_gte(length(bibentries), 5)
})
