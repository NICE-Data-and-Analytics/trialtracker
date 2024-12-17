library(testthat)
library(mockery)
loadNamespace("DBI")
loadNamespace("RSQLite")

# Define the test
test_that("update_db_for_ISRCTN_changes works correctly", {
  # Mock functions
  mock_concat_ids <- function(trial_id_df, column_name) {
    return(trial_id_df[[column_name]])
  }

  mock_generate_ISRCTN_URL <- function(ISRCTN_Id_Vector) {
    return(paste0("https://www.isrctn.com/api/query/format/who?q=", paste(ISRCTN_Id_Vector, collapse = "%20OR%20")))
  }

  mock_generate_ISRCTN_df <- function(ISRCTN_URL) {
    return(tibble::tibble(
      ISRCTN_No = c("ISRCTN12345", "ISRCTN67890"),
      Public_Title = c("Study 1", "Study 2"),
      URL = c("https://www.isrctn.com/ISRCTN12345", "https://www.isrctn.com/ISRCTN67890")
    ))
  }

  mock_update_db <- function(con, table_name, data) {
    # Mock function to simulate database update
    # print("Data to be inserted into the database:")
    # print(data)
    DBI::dbWriteTable(con, table_name, data, append = TRUE, row.names = FALSE)
  }

  # Mock external dependencies
  stub(update_db_for_ISRCTN_changes, "concat_ids", mock_concat_ids)
  stub(update_db_for_ISRCTN_changes, "generate_ISRCTN_URL", mock_generate_ISRCTN_URL)
  stub(update_db_for_ISRCTN_changes, "generate_ISRCTN_df", mock_generate_ISRCTN_df)
  stub(update_db_for_ISRCTN_changes, "update_db", mock_update_db)

  # Create a mock database connection
  mock_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(mock_con, "CREATE TABLE ISRCTN (ISRCTN_No TEXT, Public_Title TEXT, URL TEXT, Query_Date DATE, Program TEXT, `Guideline.number` TEXT)")

  # Test with valid input
  trial_id_df <- tibble::tibble(
    ISRCTN_Ids = c("ISRCTN12345", "ISRCTN67890"),
    Program = c("Program1", "Program2"),
    Guideline.number = c("G1", "G2")
  )

  update_db_for_ISRCTN_changes(
    main_con = mock_con,
    trial_id_df = trial_id_df
  )

  result <- DBI::dbReadTable(mock_con, "ISRCTN")
  # print("Database Result (Valid Input):")
  # print(result)
  expect_equal(nrow(result), 2)
  expect_equal(result$ISRCTN_No, c("ISRCTN12345", "ISRCTN67890"))
  expect_equal(result$Public_Title, c("Study 1", "Study 2"))
  expect_equal(result$URL, c("https://www.isrctn.com/ISRCTN12345", "https://www.isrctn.com/ISRCTN67890"))
  expect_equal(result$Program, c("Program1", "Program2"))
  expect_equal(result$Guideline.number, c("G1", "G2"))

  # Test with empty input
  trial_id_df_empty <- tibble::tibble(
    ISRCTN_Ids = character(),
    Program = character(),
    Guideline.number = character()
  )

  update_db_for_ISRCTN_changes(
    main_con = mock_con,
    trial_id_df = trial_id_df_empty
  )

  result_empty <- DBI::dbReadTable(mock_con, "ISRCTN")
  # print("Database Result (Empty Input):")
  # print(result_empty)
  expect_equal(nrow(result_empty), 2)  # Should remain the same as previous valid input

  # Clean up
  DBI::dbDisconnect(mock_con)
})
