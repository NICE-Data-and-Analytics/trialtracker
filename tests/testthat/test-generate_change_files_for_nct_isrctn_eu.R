library(testthat)
library(mockery)

# Mock function
mock_generate_change_files_for_all_programs_in_one_registry <- function(registry, main_con, group_cols, exclude_cols, prog_regexes, programs, daily_path) {
  message("Mock function called for registry: ", registry)
}

test_that("generate_change_files_for_nct_isrctn_eu works with multiple registry tables", {
  # Mock the dependency
  stub(generate_change_files_for_nct_isrctn_eu, "generate_change_files_for_all_programs_in_one_registry", mock_generate_change_files_for_all_programs_in_one_registry)

  # Mock inputs
  prog_regexes <- c("regex1", "regex2")
  programs <- c("program1", "program2")
  registry_tables <- c("NCT_registry", "ISRCTN_registry", "EU_registry")
  main_con <- NULL
  daily_path <- "mock/path"

  # Call the function
  generate_change_files_for_nct_isrctn_eu(prog_regexes, programs, registry_tables, main_con, daily_path)

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})

test_that("generate_change_files_for_nct_isrctn_eu works with no registry tables", {
  # Mock the dependency
  stub(generate_change_files_for_nct_isrctn_eu, "generate_change_files_for_all_programs_in_one_registry", mock_generate_change_files_for_all_programs_in_one_registry)

  # Mock inputs
  prog_regexes <- c("regex1", "regex2")
  programs <- c("program1", "program2")
  registry_tables <- character(0)
  main_con <- NULL
  daily_path <- "mock/path"

  # Call the function
  generate_change_files_for_nct_isrctn_eu(prog_regexes, programs, registry_tables, main_con, daily_path)

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})

test_that("generate_change_files_for_nct_isrctn_eu works with a single registry table", {
  # Mock the dependency
  stub(generate_change_files_for_nct_isrctn_eu, "generate_change_files_for_all_programs_in_one_registry", mock_generate_change_files_for_all_programs_in_one_registry)

  # Mock inputs
  prog_regexes <- c("regex1", "regex2")
  programs <- c("program1", "program2")
  registry_tables <- c("NCT_registry")
  main_con <- NULL
  daily_path <- "mock/path"

  # Call the function
  generate_change_files_for_nct_isrctn_eu(prog_regexes, programs, registry_tables, main_con, daily_path)

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})
