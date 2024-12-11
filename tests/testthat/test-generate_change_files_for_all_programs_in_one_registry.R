library(testthat)
library(mockery)
library(purrr)

# Mock functions
mock_pull_change <- function(registry_table, main_con, regex_pattern, start_date, end_date, group_cols, exclude_cols) {
  data.frame()
}

mock_get_last_registry_entry_before_today <- function(registry, main_con) {
  as.Date("2023-01-01")
}

mock_write_changes_to_disk <- function(data, daily_path, DF_Name, prog_name) {
  TRUE
}

test_that("generate_change_files_for_all_programs_in_one_registry works with multiple programs", {
  # Mock the dependencies
  stub(generate_change_files_for_all_programs_in_one_registry, "pull_change", mock_pull_change)
  stub(generate_change_files_for_all_programs_in_one_registry, "get_last_registry_entry_before_today", mock_get_last_registry_entry_before_today)
  stub(generate_change_files_for_all_programs_in_one_registry, "write_changes_to_disk", mock_write_changes_to_disk)

  # Mock inputs
  registry <- "test_registry"
  main_con <- NULL
  group_cols <- c("col1", "col2")
  exclude_cols <- c("col3", "col4")
  prog_regexes <- c("regex1", "regex2")
  programs <- c("program1", "program2")
  daily_path <- "mock/path"

  # Call the function
  generate_change_files_for_all_programs_in_one_registry(registry, main_con, group_cols, exclude_cols, prog_regexes, programs, daily_path)

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})

test_that("generate_change_files_for_all_programs_in_one_registry works with no programs", {
  # Mock the dependencies
  stub(generate_change_files_for_all_programs_in_one_registry, "pull_change", mock_pull_change)
  stub(generate_change_files_for_all_programs_in_one_registry, "get_last_registry_entry_before_today", mock_get_last_registry_entry_before_today)
  stub(generate_change_files_for_all_programs_in_one_registry, "write_changes_to_disk", mock_write_changes_to_disk)

  # Mock inputs
  registry <- "test_registry"
  main_con <- NULL
  group_cols <- c("col1", "col2")
  exclude_cols <- c("col3", "col4")
  prog_regexes <- character(0)
  programs <- character(0)
  daily_path <- "mock/path"

  # Call the function
  generate_change_files_for_all_programs_in_one_registry(registry, main_con, group_cols, exclude_cols, prog_regexes, programs, daily_path)

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})

test_that("generate_change_files_for_all_programs_in_one_registry works with a single program", {
  # Mock the dependencies
  stub(generate_change_files_for_all_programs_in_one_registry, "pull_change", mock_pull_change)
  stub(generate_change_files_for_all_programs_in_one_registry, "get_last_registry_entry_before_today", mock_get_last_registry_entry_before_today)
  stub(generate_change_files_for_all_programs_in_one_registry, "write_changes_to_disk", mock_write_changes_to_disk)

  # Mock inputs
  registry <- "test_registry"
  main_con <- NULL
  group_cols <- c("col1", "col2")
  exclude_cols <- c("col3", "col4")
  prog_regexes <- c("regex1")
  programs <- c("program1")
  daily_path <- "mock/path"

  # Call the function
  generate_change_files_for_all_programs_in_one_registry(registry, main_con, group_cols, exclude_cols, prog_regexes, programs, daily_path)

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})
