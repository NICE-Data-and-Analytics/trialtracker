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

test_that("download_trial_info_wrapper_no_pm_or_email works with default and custom parameters", {
  # Mock functions for updates
  mock_update_db_for_NCT_changes <- mock(function(main_con, trial_id_df) {})
  mock_update_db_for_ISRCTN_changes <- mock(function(main_con, trial_id_df) {})
  mock_update_db_for_NIHR_changes <- mock(function(main_con, trial_id_df) {})
  mock_update_db_for_EU_changes <- mock(function(main_con, trial_id_df) {})

  # Stub the original functions with the mock functions
  stub(download_trial_info_wrapper_no_pm_or_email, "update_db_for_NCT_changes", mock_update_db_for_NCT_changes)
  stub(download_trial_info_wrapper_no_pm_or_email, "update_db_for_ISRCTN_changes", mock_update_db_for_ISRCTN_changes)
  stub(download_trial_info_wrapper_no_pm_or_email, "update_db_for_NIHR_changes", mock_update_db_for_NIHR_changes)
  stub(download_trial_info_wrapper_no_pm_or_email, "update_db_for_EU_changes", mock_update_db_for_EU_changes)

  #Test trial_id_df
  trial_id_df <- tibble::tibble(
      Program = c("Program A", "Program B"),
      Guideline.number = c("G1", "G2"),
      URL = c("http://example.com/a", "http://example.com/b"),
      EU_Ids = c("2024-123456-12", "2023-654321-34"),
      `Short..working.title.` = c('Bibbety Bobbity', 'Boo'),
      NCT_Ids = c('NCT12345678', 'NCT87654321'),  # Ensure this column exists
      ISRCTN_Ids = c('ISRCTN123456', 'ISRCTN654321'),
      NIHR_Ids = c('X1234', 'ABC765')
    )

  # Test with mocked DBI functions (default parameters)
  download_trial_info_wrapper_no_pm_or_email(main_con = RSQLite::dbConnect(RSQLite::SQLite(), ":memory:"),
                                             trial_id_df = trial_id_df)

  # Verify update functions were called
  expect_called(mock_update_db_for_NCT_changes, 1)
  expect_called(mock_update_db_for_ISRCTN_changes, 1)
  expect_called(mock_update_db_for_NIHR_changes, 1)
  expect_called(mock_update_db_for_EU_changes, 1)
})


