library(testthat)

test_that("concat_ids works correctly", {
  # Test 1: Basic Functionality
  df1 <- data.frame(id = c("NCT1234567", "NCT987654321", NA, "NCT1234567"))
  expect_equal(concat_ids(df1, "id"), c("NCT1234567", "NCT987654321"))

  # Test 2: Empty Dataframe
  df2 <- data.frame(id = character(0))
  expect_equal(concat_ids(df2, "id"), character(0))

  # Test 3: No Valid IDs
  df3 <- data.frame(id = c(NA, NA, ""))
  expect_equal(concat_ids(df3, "id"), character(0))

  # Test 4: All Unique IDs
  df4 <- data.frame(id = c("NCT1111111", "NCT2222222", "NCT3333333"))
  expect_equal(concat_ids(df4, "id"), c("NCT1111111", "NCT2222222", "NCT3333333"))

  # Test 5: Column Not Found
  df5 <- data.frame(other_col = c("NCT1234567", "NCT987654321"))
  expect_error(concat_ids(df5, "id"), "Column id not found in dataframe")

})
