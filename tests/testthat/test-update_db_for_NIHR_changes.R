library(testthat)

# Mock functions
mock_fromJSON <- function(url) {
  data.frame(
    project_id = c("123", "456"),
    project_title = c("Project A", "Project B"),
    project_status = c("Active", "Completed"),
    end_date = as.Date(c("2024-12-31", "2023-12-31"))
  )
}

mock_update_db <- function(con, table_name, df) {
  assign("last_update_db_call", list(con = con, table_name = table_name, df = df), envir = .GlobalEnv)
}

# Replace the real functions with mocks
assignInNamespace("fromJSON", mock_fromJSON, ns = "jsonlite")
assignInNamespace("update_db", mock_update_db, ns = "trialtracker")

# Sample data
trial_id_df <- data.frame(
  Program = c("Program A", "Program B"),
  Guideline.number = c("G1", "G2"),
  URL = c("http://example.com/a", "http://example.com/b"),
  NIHR_Ids = c("123", "456")
)

# Test function
test_that("update_db_for_NIHR_changes works correctly", {
  main_con <- NULL  # Mock database connection

  update_db_for_NIHR_changes(main_con, trial_id_df)

  # Verify the update_db call
  expect_true(exists("last_update_db_call"))
  expect_equal(last_update_db_call$con, main_con)
  expect_equal(last_update_db_call$table_name, "NIHR")
  expect_equal(nrow(last_update_db_call$df), 2)
  expect_equal(last_update_db_call$df$project_id, c("123", "456"))
})

# Clean up
rm(last_update_db_call, envir = .GlobalEnv)
