library(testthat)
library(DBI)
library(RSQLite)

test_that("update_db works correctly", {
  # Create an in-memory SQLite database
  main_con <- dbConnect(RSQLite::SQLite(), ":memory:")

  # Test 1: Table Creation
  DF1 <- data.frame(Query_Date = as.numeric(Sys.Date()-1), Value = 1:5)
  update_db(main_con, "example_registry", DF1)
  expect_true("example_registry" %in% dbListTables(main_con))
  expect_equal(dbReadTable(main_con, "example_registry"), DF1)

  # Test 2: Append Data to Existing Table
  DF2 <- data.frame(Query_Date = as.numeric(Sys.Date()), Value = 6:10)
  update_db(main_con, "example_registry", DF2)
  expected_df2 <- rbind(DF1, DF2)
  expect_equal(dbReadTable(main_con, "example_registry"), expected_df2)

  # Test 3: Overwrite and Append Data
  DF3 <- data.frame(Query_Date = as.numeric(Sys.Date()), Value = 11:15)
  update_db(main_con, "example_registry", DF3)
  expected_df3 <- rbind(DF1, DF3)
  expect_equal(dbReadTable(main_con, "example_registry"), expected_df3)

  # Test 4: Handle Null Dataframe
  update_db(main_con, "example_registry", NULL)
  expect_equal(dbReadTable(main_con, "example_registry"), expected_df3)

  # Clean up
  dbDisconnect(main_con)
})
