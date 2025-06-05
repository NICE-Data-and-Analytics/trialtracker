library(testthat)
library(mockery)
loadNamespace("DBI")
loadNamespace("RSQLite")

# Define the test
test_that("update_db_for_NCT_changes works correctly", {
  # Mock functions
  mock_concat_ids <- function(trial_id_df, column_name) {
    trial_id_df[[column_name]]
  }

  mock_generate_NCT_URL <- function(NCT_Id_Vector) {
    paste0("https://clinicaltrials.gov/api/query/full_studies?expr=", paste(NCT_Id_Vector, collapse = "+OR+"))
  }

  mock_generate_NCT_DF <- function(NCT_URL) {
    tibble::tibble(
      NCTId = c("NCT123", "NCT456"),
      Title = c("Study 1", "Study 2"),
      Status = c("Completed", "Recruiting"),
      Rank = c(1,2)
    )
  }

  mock_update_db <- function(con, table_name, data) {
    # Mock function to simulate database update
    DBI::dbWriteTable(con, table_name, data, append = TRUE, row.names = FALSE)
  }

  # Mock external dependencies
  stub(update_db_for_NCT_changes, "concat_ids", mock_concat_ids)
  stub(update_db_for_NCT_changes, "generate_NCT_URL", mock_generate_NCT_URL)
  stub(update_db_for_NCT_changes, "generate_NCT_DF", mock_generate_NCT_DF)
  stub(update_db_for_NCT_changes, "update_db", mock_update_db)

  # Create a mock database connection
  mock_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(mock_con, "CREATE TABLE NCT (NCTId TEXT, Title TEXT, Status TEXT, Query_Date DATE, Program TEXT, `Guideline.number` TEXT, URL TEXT Rank INT)")

  # Create a mock trial_id_df with all necessary columns
  trial_id_df <- tibble::tibble(
    NCT_Ids = c("NCT123", "NCT456"),
    Program = c("Program1", "Program2"),
    Guideline.number = c("G1", "G2"),
    URL = c("http://example.com/1", "http://example.com/2")
  )

  # Call the function with the mocked dependencies
  update_db_for_NCT_changes(
    main_con = mock_con,
    trial_id_df = trial_id_df
  )

  # Check if the database was updated correctly
  result <- DBI::dbReadTable(mock_con, "NCT")
  expect_equal(nrow(result), 2)
  expect_equal(result$NCTId, c("NCT123", "NCT456"))
  expect_equal(result$Title, c("Study 1", "Study 2"))
  expect_equal(result$Status, c("Completed", "Recruiting"))
  expect_equal(result$Program, c("Program1", "Program2"))
  expect_equal(result$Guideline.number, c("G1", "G2"))
  expect_equal(result$URL, c("http://example.com/1", "http://example.com/2"))

  # Clean up
  DBI::dbDisconnect(mock_con)
})
