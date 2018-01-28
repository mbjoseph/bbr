context("zzz")

test_that("Maybe as numeric makes numbers from strings", {
  expect_equal(maybe_as_numeric('5'), 5)
})

test_that("Maybe as numeric leaves letters as strings", {
  expect_equal(maybe_as_numeric('a'), 'a')
})

test_that("Empty string to NA creates NA from empty string", {
  expect_identical(empty_string_to_na(''), NA)
})

test_that("clean_colnames raises error with non data frame arg", {
  expect_error(clean_colnames(1:5))
})

test_that('clean_colnames removes rank column', {
  test_df <- data.frame(Rk = 1, dummy = 1)
  clean_df <- clean_colnames(test_df)
  expect_equal(colnames(clean_df), 'dummy')
})
