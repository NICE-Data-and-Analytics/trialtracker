library(testthat)
library(mockery)

# Mock data
mock_new_data <- data.frame(
  Guideline.number = c("GL123", "GL456"),
  project_id = c(1, 2),
  Query_Date = as.Date(c("2023-01-01", "2023-01-02")),
  URL = c("url1", "url2"),
  project_title = c("title1", "title2"),
  stringsAsFactors = FALSE
)

mock_old_data <- data.frame(
  Guideline.number = c("GL123", "GL456"),
  project_id = c(1, 2),
  Query_Date = as.Date(c("2022-12-01", "2022-12-02")),
  URL = c("url1", "url2"),
  project_title = c("title1", "title2"),
  stringsAsFactors = FALSE
)

mock_comparison <- data.frame(
  Guideline.number = c("GL123", "GL456"),
  project_id = c(1, 2),
  Change_Type = c("No Change", "No Change"),
  stringsAsFactors = FALSE
)

# Mock functions
mock_pull_nihr_change <- function(prog_regex, old_or_new, main_con) {
  if (old_or_new == "new") {
    mock_new_data
  } else {
    mock_old_data
  }
}

mock_compare_df <- function(df_new, df_old, group_col, exclude, stop_on_error) {
  mock_comparison
}

mock_write_changes_to_disk <- function(comparison, daily_path, DF_Name, prog_name) {
  TRUE
}

test_that("generate_nihr_comparison_table works with valid inputs", {
  # Mock the dependencies
  stub(generate_nihr_comparison_table, "pull_nihr_change", mock_pull_nihr_change)
  stub(generate_nihr_comparison_table, "compareDF::compare_df", mock_compare_df)
  stub(generate_nihr_comparison_table, "write_changes_to_disk", mock_write_changes_to_disk)

  # Mock database connection
  main_con <- NULL
  daily_path <- "mock/path"

  # Call the function
  generate_nihr_comparison_table("Test Program", "GL.*", main_con, daily_path)

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})

test_that("generate_nihr_comparison_table handles empty dataframes", {
  # Mock the dependencies to return empty dataframes
  stub(generate_nihr_comparison_table, "pull_nihr_change", function(...) data.frame())
  stub(generate_nihr_comparison_table, "compareDF::compare_df", function(...) data.frame())
  stub(generate_nihr_comparison_table, "write_changes_to_disk", mock_write_changes_to_disk)

  # Mock database connection
  main_con <- NULL
  daily_path <- "mock/path"

  # Call the function
  generate_nihr_comparison_table("Test Program", "GL.*", main_con, daily_path)

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})
