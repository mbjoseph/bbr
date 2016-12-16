context("get_players")

test_that("Non-letter initials raise errors", {
  expect_error(get_players(initial = 1))
})

test_that("Multiple initials raise errors", {
  expect_error(get_players(initial = c("A", "B")))
})

test_that("get_players returns the expected number of columns", {
  a_data <- get_players("a")
  expect_equal(ncol(a_data), 8)
})
