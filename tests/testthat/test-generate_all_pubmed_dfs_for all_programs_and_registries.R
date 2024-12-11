library(testthat)
library(mockery)
library(purrr)

# Mock function
mock_generate_all_pubmed_dfs_for_all_programs_one_registry <- function(registry, prog_regexes, programs, main_con) {
  message("Mock function called for registry: ", registry)
}

test_that("generate_all_pubmed_dfs_for_all_programs_and_registries works with multiple registry tables", {
  # Mock the dependency
  stub(generate_all_pubmed_dfs_for_all_programs_and_registries, "generate_all_pubmed_dfs_for_all_programs_one_registry", mock_generate_all_pubmed_dfs_for_all_programs_one_registry)

  # Mock inputs
  registry_tables <- c("registry1", "registry2")
  prog_regexes <- c("regex1", "regex2")
  programs <- c("program1", "program2")
  main_con <- NULL

  # Call the function
  generate_all_pubmed_dfs_for_all_programs_and_registries(registry_tables, prog_regexes, programs, main_con)

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})

test_that("generate_all_pubmed_dfs_for_all_programs_and_registries works with no registry tables", {
  # Mock the dependency
  stub(generate_all_pubmed_dfs_for_all_programs_and_registries, "generate_all_pubmed_dfs_for_all_programs_one_registry", mock_generate_all_pubmed_dfs_for_all_programs_one_registry)

  # Mock inputs
  registry_tables <- character(0)
  prog_regexes <- c("regex1", "regex2")
  programs <- c("program1", "program2")
  main_con <- NULL

  # Call the function
  generate_all_pubmed_dfs_for_all_programs_and_registries(registry_tables, prog_regexes, programs, main_con)

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})

test_that("generate_all_pubmed_dfs_for_all_programs_and_registries works with a single registry table", {
  # Mock the dependency
  stub(generate_all_pubmed_dfs_for_all_programs_and_registries, "generate_all_pubmed_dfs_for_all_programs_one_registry", mock_generate_all_pubmed_dfs_for_all_programs_one_registry)

  # Mock inputs
  registry_tables <- c("registry1")
  prog_regexes <- c("regex1", "regex2")
  programs <- c("program1", "program2")
  main_con <- NULL

  # Call the function
  generate_all_pubmed_dfs_for_all_programs_and_registries(registry_tables, prog_regexes, programs, main_con)

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})
