library(testthat)
library(mockery)
library(DBI)
library(RSQLite)
library(dplyr)

# Define the test
test_that("update_all_pubmed_tables works correctly", {
  # Mock function
  mock_generate_pubmed_results <- function(registry, trial_id_df, main_con, mindate, maxdate) {
    # Mock function to simulate updating PubMed tables - commented out for test report
    print(paste("Updating PubMed table for registry:", registry))
  }

  # Mock external dependency
  stub(update_all_pubmed_tables, "generate_pubmed_results_from_search_terms_and_update_db_one_registry", mock_generate_pubmed_results)

  # Create a mock database connection
  mock_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  # Create a mock trial_id_df
  trial_id_df <- tibble::tibble(
    ISRCTN_Ids = c("12345", "67890"),
    NIHR_Ids = c("NIHR123", "NIHR456"),
    EU_Ids = c("EU123", "EU456"),
    NCT_Ids = c("NCT123", "NCT456"),
    Program = c("Program1", "Program2"),
    Guideline.number = c("G1", "G2")
  )

  # Call the function with the mocked dependencies
  update_all_pubmed_tables(
    registries = c("NCT", "ISRCTN", "NIHR", "EU"),
    trial_id_df = trial_id_df,
    main_con = mock_con
  )

  # Check if the function was called for each registry
  expect_output(update_all_pubmed_tables(
    registries = c("NCT", "ISRCTN", "NIHR", "EU"),
    trial_id_df = trial_id_df,
    main_con = mock_con
  ), "Updating PubMed table for registry: NCT")
  expect_output(update_all_pubmed_tables(
    registries = c("NCT", "ISRCTN", "NIHR", "EU"),
    trial_id_df = trial_id_df,
    main_con = mock_con
  ), "Updating PubMed table for registry: ISRCTN")
  expect_output(update_all_pubmed_tables(
    registries = c("NCT", "ISRCTN", "NIHR", "EU"),
    trial_id_df = trial_id_df,
    main_con = mock_con
  ), "Updating PubMed table for registry: NIHR")
  expect_output(update_all_pubmed_tables(
    registries = c("NCT", "ISRCTN", "NIHR", "EU"),
    trial_id_df = trial_id_df,
    main_con = mock_con
  ), "Updating PubMed table for registry: EU")

  # Clean up
  DBI::dbDisconnect(mock_con)
})
