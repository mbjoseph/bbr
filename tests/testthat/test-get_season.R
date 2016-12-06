context("get_season")

test_that("Years before 1947 raise errors", {
  expect_error(get_season(year = 1946))
})

test_that("Years in the future raise errors", {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  expect_error(get_season(year = current_year + 3))
})

test_that("get_season returns the expected number of rows & cols for 2010", {
  twentyten_data <- get_season(2010)
  expect_equal(nrow(twentyten_data), 578)
  expect_equal(ncol(twentyten_data), 31)
})

test_that("maybe_as_numeric converts chars to numbers only when appropriate", {
  should_convert <- c("3", "2")
  should_not_convert <- c("foo", "bar")
  expect_is(maybe_as_numeric(should_convert), "numeric")
  expect_is(maybe_as_numeric(should_not_convert), "character")
})
