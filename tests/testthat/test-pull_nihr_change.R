library(testthat)
library(mockery)

# Reset Sys.Date() to the current date
Sys.setenv(TZ = "UTC") # Ensure consistent timezone
Sys.Date <- function() base::Sys.Date()

# Mock data
mock_data <- data.frame(
  Query_Date = as.Date(c("2023-01-01", "2023-01-02", as.character(Sys.Date()), as.character(Sys.Date()))),
  Guideline.number = c("GL123", "GL456", "GL789", "GL101"),
  stringsAsFactors = FALSE
)

# Mock functions
mock_dbReadTable <- function(con, table) {
  mock_data
}

mock_get_last_registry_entry_before_today <- function(table, con) {
  as.Date("2023-01-01")
}

test_that("pull_nihr_change works for old data", {
  # Mock the dependencies
  stub(pull_nihr_change, "DBI::dbReadTable", mock_dbReadTable)
  stub(pull_nihr_change, "get_last_registry_entry_before_today", mock_get_last_registry_entry_before_today)

  # Mock database connection
  main_con <- NULL

  # Call the function
  result <- pull_nihr_change(prog_regex = "GL123", old_or_new = "old", main_con = main_con)

  # Assertions
  expect_equal(nrow(result), 1)
  expect_equal(result$Guideline.number, "GL123")
  expect_equal(result$Query_Date, as.Date("2023-01-01"))
})

test_that("pull_nihr_change works for new data", {
  # Mock the dependencies
  stub(pull_nihr_change, "DBI::dbReadTable", mock_dbReadTable)

  # Mock database connection
  main_con <- NULL

  # Call the function
  result <- pull_nihr_change(prog_regex = "GL789", old_or_new = "new", main_con = main_con)

  # Assertions
  expect_equal(nrow(result), 1)
  expect_equal(result$Guideline.number, "GL789")
  expect_equal(result$Query_Date, Sys.Date())
})

test_that("pull_nihr_change handles no matching data", {
  # Mock the dependencies
  stub(pull_nihr_change, "DBI::dbReadTable", mock_dbReadTable)
  stub(pull_nihr_change, "get_last_registry_entry_before_today", mock_get_last_registry_entry_before_today)

  # Mock database connection
  main_con <- NULL

  # Call the function
  result <- pull_nihr_change(prog_regex = "GL999", old_or_new = "old", main_con = main_con)

  # Assertions
  expect_equal(nrow(result), 0)

})
