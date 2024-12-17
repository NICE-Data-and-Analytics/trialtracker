library(testthat)
library(mockery)

# Mock function
mock_generate_nihr_comparison_table <- function(program, program_regex, main_con, daily_path) {
  message("Mock function called for program: ", program)
}

test_that("generate_change_files_for_nihr works with multiple programs", {
  # Mock the dependency
  stub(generate_change_files_for_nihr, "generate_nihr_comparison_table", mock_generate_nihr_comparison_table)

  # Mock inputs
  prog_regexes <- c("regex1", "regex2")
  programs <- c("program1", "program2")
  main_con <- NULL
  daily_path <- "mock/path"

  # Call the function
  generate_change_files_for_nihr(prog_regexes, programs, main_con, daily_path)

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})

test_that("generate_change_files_for_nihr works with no programs", {
  # Mock the dependency
  stub(generate_change_files_for_nihr, "generate_nihr_comparison_table", mock_generate_nihr_comparison_table)

  # Mock inputs
  prog_regexes <- character(0)
  programs <- character(0)
  main_con <- NULL
  daily_path <- "mock/path"

  # Call the function
  generate_change_files_for_nihr(prog_regexes, programs, main_con, daily_path)

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})

test_that("generate_change_files_for_nihr works with a single program", {
  # Mock the dependency
  stub(generate_change_files_for_nihr, "generate_nihr_comparison_table", mock_generate_nihr_comparison_table)

  # Mock inputs
  prog_regexes <- c("regex1")
  programs <- c("program1")
  main_con <- NULL
  daily_path <- "mock/path"

  # Call the function
  generate_change_files_for_nihr(prog_regexes, programs, main_con, daily_path)

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})
