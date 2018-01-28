context("get_player_data")

test_that("Numeric slugs raise errors", {
  expect_error(get_player_data(slug = 2342341))
})

test_that("Multiple slugs raise errors", {
  expect_error(get_player_data(slug = c('abdelal01', 'jordami01')))
})

test_that("Non-existent slugs raise errors", {
  expect_error(get_player_data(slug = 'some-fake-slug'))
})

test_that("get_player_data returns the expected number of columns", {
  jordan_data <- get_player_data("jordami01")
  expect_equal(ncol(jordan_data), 32)
})
