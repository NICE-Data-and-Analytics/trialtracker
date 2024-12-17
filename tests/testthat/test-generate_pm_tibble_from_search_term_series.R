library(testthat)
library(mockery)
loadNamespace("DBI")
loadNamespace("RSQLite")
library(dplyr)

# Define the test
test_that("generate_pubmed_results_from_search_terms_and_update_db_one_registry works correctly", {
  # Mock functions
  mock_create_search_list <- function(trial_id_df, registry) {
    tibble::tibble(ID = c("12345", "67890"))
  }

  mock_generate_pm_tibble_from_search_term_series <- function(search_list, api_object, mindate, maxdate) {
    tibble::tibble(
      ID = c("12345", "67890"),
      Title = c("Study 1", "Study 2"),
      Date = c("2023-01-01", "2023-02-01")
    )
  }

  mock_update_db <- function(con, table_name, data) {
    # Debugging print statement
    # print("Data to be inserted into the database:")
    # print(data)

    # Insert data into the database
    DBI::dbWriteTable(con, table_name, data, append = TRUE, row.names = FALSE)
  }

  # Mock external dependencies
  stub(generate_pubmed_results_from_search_terms_and_update_db_one_registry, "create_search_list", mock_create_search_list)
  stub(generate_pubmed_results_from_search_terms_and_update_db_one_registry, "generate_pm_tibble_from_search_term_series", mock_generate_pm_tibble_from_search_term_series)
  stub(generate_pubmed_results_from_search_terms_and_update_db_one_registry, "update_db", mock_update_db)
  stub(generate_pubmed_results_from_search_terms_and_update_db_one_registry, "readr::read_file", function(...) "mock_api_key")

  # Create a mock database connection
  mock_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(mock_con, "CREATE TABLE ISRCTN_PM (ID TEXT, Title TEXT, Date TEXT, Program TEXT, `Guideline.number` TEXT)")

  # Create a mock trial_id_df with all necessary columns
  trial_id_df <- tibble::tibble(
    ISRCTN_Ids = c("12345", "67890"),
    NIHR_Ids = c("NIHR123", "NIHR456"),
    EU_Ids = c("EU123", "EU456"),
    NCT_Ids = c("NCT123", "NCT456"),
    Program = c("Program1", "Program2"),
    Guideline.number = c("G1", "G2")
  )

  # Call the function with the mocked dependencies
  generate_pubmed_results_from_search_terms_and_update_db_one_registry(
    registry = "ISRCTN",
    trial_id_df = trial_id_df,
    main_con = mock_con,
    mindate = "2023-01-01",
    maxdate = "2023-02-01"
  )

  # Debugging print statements
  # search_list <- mock_create_search_list(trial_id_df, "ISRCTN")
  # print("Search List:")
  # print(search_list)

  pm_tibble <- mock_generate_pm_tibble_from_search_term_series(search_list, "mock_api_key", "2023-01-01", "2023-02-01")
  # print("PM Tibble:")
  # print(pm_tibble)

  if (nrow(pm_tibble) > 0) {
    pm_tibble <- pm_tibble |>
      dplyr::left_join(trial_id_df, by = c("ID" = "ISRCTN_Ids")) |>
      dplyr::select(Program, Guideline.number, dplyr::everything()) |>
      dplyr::select(-all_of(c("NIHR_Ids", "EU_Ids", "NCT_Ids"))) |>
      dplyr::distinct()
    #print("Joined Tibble:")
    #print(pm_tibble)
  } else {
    print("PM Tibble is empty.")
  }

  # Check if the database was updated correctly
  result <- DBI::dbReadTable(mock_con, "ISRCTN_PM")
  # print("Database Result:")
  # print(result)
  expect_equal(nrow(result), 2)
  expect_equal(result$ID, c("12345", "67890"))
  expect_equal(result$Title, c("Study 1", "Study 2"))
  expect_equal(result$Date, c("2023-01-01", "2023-02-01"))
  expect_equal(result$Program, c("Program1", "Program2"))
  expect_equal(result$Guideline.number, c("G1", "G2"))

  # Clean up
  DBI::dbDisconnect(mock_con)
})
