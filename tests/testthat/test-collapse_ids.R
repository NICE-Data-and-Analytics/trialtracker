library(testthat)

test_that("collapse_ids works correctly", {
  # Test 1: Basic Functionality
  df1 <- data.frame(id = c("NCT1234567", "NCT987654321", NA, "NCT1234567"))
  expect_equal(collapse_ids(df1, "id", ","), "NCT1234567,NCT987654321")

  # Test 2: Empty Dataframe
  df2 <- data.frame(id = character(0))
  expect_equal(collapse_ids(df2, "id", ","), "")

  # Test 3: No Valid IDs
  df3 <- data.frame(id = c(NA, NA, ""))
  expect_equal(collapse_ids(df3, "id", ","), "")

  # Test 4: All Unique IDs
  df4 <- data.frame(id = c("NCT1111111", "NCT2222222", "NCT3333333"))
  expect_equal(collapse_ids(df4, "id", ","), "NCT1111111,NCT2222222,NCT3333333")

  # Test 5: Column Not Found
  df5 <- data.frame(other_col = c("NCT1234567", "NCT987654321"))
  expect_error(collapse_ids(df5, "id", ","), "Column id not found in dataframe")

  # Test 6: Different Delimiters
  df6 <- data.frame(id = c("NCT1234567", "NCT987654321"))
  expect_equal(collapse_ids(df6, "id", ";"), "NCT1234567;NCT987654321")
  expect_equal(collapse_ids(df6, "id", "|"), "NCT1234567|NCT987654321")
})
