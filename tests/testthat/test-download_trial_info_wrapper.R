library(testthat)
library(mockery)
loadNamespace("tibble")
loadNamespace("RSQLite")

test_that("download_trial_info_wrapper calls the correct functions with the correct arguments", {
  # Create mock functions
  mock_download_trial_info_wrapper_no_pm_or_email <- mock()
  mock_update_all_pubmed_tables <- mock()
  mock_generate_email_alerts <- mock()
  mock_quarto_render <- mock(function(...) NULL)

  # Stub the original functions with the mock functions
  stub(download_trial_info_wrapper, "download_trial_info_wrapper_no_pm_or_email", mock_download_trial_info_wrapper_no_pm_or_email)
  stub(download_trial_info_wrapper, "update_all_pubmed_tables", mock_update_all_pubmed_tables)
  stub(download_trial_info_wrapper, "generate_email_alerts", mock_generate_email_alerts)
  stub(download_trial_info_wrapper, "quarto::quarto_render", mock_quarto_render)

  # Test trial_id_df
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

  # Test with mocked DBI functions (default parameters)
  main_con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
  dev_flag <- TRUE

  download_trial_info_wrapper(
    main_con = main_con,
    trial_id_df = trial_id_df,
    dev_flag = dev_flag
  )

  # Verify the functions were called with the correct arguments
  expect_called(mock_download_trial_info_wrapper_no_pm_or_email, 1)
  expect_called(mock_update_all_pubmed_tables, 1)
  expect_called(mock_generate_email_alerts, 1)

  expect_args(mock_update_all_pubmed_tables, 1, main_con, trial_id_df)
  expect_args(mock_generate_email_alerts, 1, dev_flag)
})
