library(testthat)
library(dplyr)
library(httr2)
library(ctrdata)
library(stringr)
library(nodbi)
library(mockery)
library(tibble)
library(DBI)
library(RSQLite)

# Sample data
trial_id_df <- tibble::tibble(
  Program = c("Program A", "Program B"),
  Guideline.number = c("G1", "G2"),
  URL = c("http://example.com/a", "http://example.com/b"),
  EU_Ids = c("2024-123456-12", "2023-654321-34"),
  `Short..working.title.` = c('Bibbety Bobbity', 'Boo'),
  NCT_Ids = c('NCT12345678', 'NCT87654321'),
  ISRCTN_Ids = c('ISRCTN123456', 'ISRCTN654321'),
  NIHR_Ids = c('X1234', 'ABC765')
)

test_that("update_db_for_EU_changes works correctly", {
  # Create the global variable
  if (exists("last_update_db_call", envir = .GlobalEnv)) {
    rm(last_update_db_call, envir = .GlobalEnv)
  }

  last_update_db_call <<- NULL

  # Mock functions
  mock_req_perform <- function(req) {
    #print("Mock req_perform called")
    response <- structure(list(
      status_code = 200,
      content = charToRaw('{"results": []}')
    ), class = "httr2_response")
    return(response)
  }

  mock_ctrLoadQueryIntoDb <- function(queryterm, con) {
    #print("Mock ctrLoadQueryIntoDb called")
    # Do nothing
  }

  mock_dbGetFieldsIntoDf <- function(fields, con) {
    #print("Mock dbGetFieldsIntoDf called")
    df <- tibble::tibble(
      `_id` = c("2024-123456-12", "2023-654321-34"),
      end_of_trial = c("2024-12-31", "2023-12-31"),
      a3 = c("Title A", "Title B"),
      a4 = c("Status A", "Status B")
    )
    return(df)
  }

  mock_update_db <- function(con, table_name, df) {
    #print("Mock update_db called")
    last_update_db_call <<- list(con = con, table_name = table_name, df = df)
    #print("last_update_db_call assigned")
  }

  mock_src_sqlite <- function(dbname, collection) {
    #print("Mock src_sqlite called")
    structure(list(), class = "nodbi_src_sqlite")
  }

  # Use mockery to temporarily replace functions
  stub(trialtracker::update_db_for_EU_changes, "httr2::req_perform", mock_req_perform)
  stub(trialtracker::update_db_for_EU_changes, "ctrdata::ctrLoadQueryIntoDb", mock_ctrLoadQueryIntoDb)
  stub(trialtracker::update_db_for_EU_changes, "ctrdata::dbGetFieldsIntoDf", mock_dbGetFieldsIntoDf)
  stub(trialtracker::update_db_for_EU_changes, "update_db", mock_update_db)
  stub(trialtracker::update_db_for_EU_changes, "nodbi::src_sqlite", mock_src_sqlite)

  # Create a mock database connection
  main_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  #print("Calling update_db_for_EU_changes")
  trialtracker::update_db_for_EU_changes(main_con, trial_id_df)

  # Verify the update_db call
  #print("Verifying update_db call")
  expect_true(exists("last_update_db_call", envir = .GlobalEnv))
  expect_equal(last_update_db_call$con, main_con)
  expect_equal(last_update_db_call$table_name, "EU")
  expect_equal(nrow(last_update_db_call$df), 2)
  expect_equal(last_update_db_call$df$`X_id`, c("2024-123456-12", "2023-654321-34"))

  # Clean up
  rm(last_update_db_call, envir = .GlobalEnv)
  DBI::dbDisconnect(main_con)
})
