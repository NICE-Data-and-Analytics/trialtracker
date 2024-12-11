library(testthat)
library(mockery)

test_that("generate_all_pubmed_dfs_for_all_programs_one_registry works with multiple programs", {
  # Mock the dependencies
  stub(generate_all_pubmed_dfs_for_all_programs_one_registry, "generate_pubmed_df_for_email_attachment", function(...) data.frame())
  stub(generate_all_pubmed_dfs_for_all_programs_one_registry, "get_last_registry_entry_before_today", function(...) as.Date("2023-01-01"))
  stub(generate_all_pubmed_dfs_for_all_programs_one_registry, "write_pubmed_dfs_to_disk", function(...) TRUE)

  # Mock database connection
  main_con <- NULL

  # Call the function
  generate_all_pubmed_dfs_for_all_programs_one_registry(
    registry = "test_registry",
    prog_regexes = c("regex1", "regex2"),
    programs = c("program1", "program2"),
    main_con = main_con
  )

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})

test_that("generate_all_pubmed_dfs_for_all_programs_one_registry works with no programs", {
  # Mock the dependencies
  stub(generate_all_pubmed_dfs_for_all_programs_one_registry, "generate_pubmed_df_for_email_attachment", function(...) data.frame())
  stub(generate_all_pubmed_dfs_for_all_programs_one_registry, "get_last_registry_entry_before_today", function(...) as.Date("2023-01-01"))
  stub(generate_all_pubmed_dfs_for_all_programs_one_registry, "write_pubmed_dfs_to_disk", function(...) TRUE)

  # Mock database connection
  main_con <- NULL

  # Call the function
  generate_all_pubmed_dfs_for_all_programs_one_registry(
    registry = "test_registry",
    prog_regexes = character(0),
    programs = character(0),
    main_con = main_con
  )

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})

test_that("generate_all_pubmed_dfs_for_all_programs_one_registry works with a single program", {
  # Mock the dependencies
  stub(generate_all_pubmed_dfs_for_all_programs_one_registry, "generate_pubmed_df_for_email_attachment", function(...) data.frame())
  stub(generate_all_pubmed_dfs_for_all_programs_one_registry, "get_last_registry_entry_before_today", function(...) as.Date("2023-01-01"))
  stub(generate_all_pubmed_dfs_for_all_programs_one_registry, "write_pubmed_dfs_to_disk", function(...) TRUE)

  # Mock database connection
  main_con <- NULL

  # Call the function
  generate_all_pubmed_dfs_for_all_programs_one_registry(
    registry = "test_registry",
    prog_regexes = c("regex1"),
    programs = c("program1"),
    main_con = main_con
  )

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})
