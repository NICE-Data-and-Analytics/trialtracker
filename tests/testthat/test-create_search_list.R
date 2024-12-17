library(testthat)

test_that("create_search_list works correctly", {
  # Test 1: Basic Functionality
  trial_id_df1 <- data.frame(CT_Ids = c("ID1", "ID2", NA, "ID1"))
  expect_equal(create_search_list(trial_id_df1, "CT"), c("ID1", "ID2"))

  # Test 2: Empty Dataframe
  trial_id_df2 <- data.frame(CT_Ids = character(0))
  expect_equal(create_search_list(trial_id_df2, "CT"), character(0))

  # Test 3: No Valid IDs
  trial_id_df3 <- data.frame(CT_Ids = c(NA, NA, ""))
  expect_equal(create_search_list(trial_id_df3, "CT"), character(0))

  # Test 4: All Unique IDs
  trial_id_df4 <- data.frame(CT_Ids = c("ID1", "ID2", "ID3"))
  expect_equal(create_search_list(trial_id_df4, "CT"), c("ID1", "ID2", "ID3"))

  # Test 5: Column Not Found
  trial_id_df5 <- data.frame(Other_Ids = c("ID1", "ID2"))
  expect_error(create_search_list(trial_id_df5, "CT"), "Column CT_Ids not found in dataframe")
})
