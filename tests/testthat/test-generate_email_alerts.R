library(testthat)
library(mockery)
library(emayili)
library(readr)
library(DBI)
library(RSQLite)

# Mock functions
mock_generate_change_files_for_nct_isrctn_eu <- function(prog_regexes, programs, registry_tables, main_con, daily_path) {
  message("Mock generate_change_files_for_nct_isrctn_eu called")
}

mock_generate_change_files_for_nihr <- function(prog_regexes, programs, main_con, daily_path) {
  message("Mock generate_change_files_for_nihr called")
}

mock_generate_all_pubmed_dfs_for_all_programs_and_registries <- function(registry_tables, prog_regexes, programs, main_con) {
  message("Mock generate_all_pubmed_dfs_for_all_programs_and_registries called")
}

mock_load_attachments_and_send_email_alert_for_all_programs <- function(programs, daily_path, dev_flag, smtp) {
  message("Mock load_attachments_and_send_email_alert_for_all_programs called")
}

mock_smtp_server <- function(...) {
  message("Mock SMTP server setup")
  structure(list(), class = "smtp_server")
}

test_that("generate_email_alerts works with dev_flag set to TRUE", {
  # Mock the dependencies
  stub(generate_email_alerts, "generate_change_files_for_nct_isrctn_eu", mock_generate_change_files_for_nct_isrctn_eu)
  stub(generate_email_alerts, "generate_change_files_for_nihr", mock_generate_change_files_for_nihr)
  stub(generate_email_alerts, "generate_all_pubmed_dfs_for_all_programs_and_registries", mock_generate_all_pubmed_dfs_for_all_programs_and_registries)
  stub(generate_email_alerts, "load_attachments_and_send_email_alert_for_all_programs", mock_load_attachments_and_send_email_alert_for_all_programs)
  stub(generate_email_alerts, "emayili::server", mock_smtp_server)
  stub(generate_email_alerts, "readr::read_file", function(...) "mock_password")
  stub(generate_email_alerts, "DBI::dbConnect", function(...) structure(list(), class = "DBIConnection"))
  stub(generate_email_alerts, "DBI::dbDisconnect", function(...) TRUE)

  # Call the function
  generate_email_alerts(TRUE, base_path = testthat::test_path("data/email_attachments"))

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})

test_that("generate_email_alerts works with dev_flag set to FALSE", {
  # Mock the dependencies
  stub(generate_email_alerts, "generate_change_files_for_nct_isrctn_eu", mock_generate_change_files_for_nct_isrctn_eu)
  stub(generate_email_alerts, "generate_change_files_for_nihr", mock_generate_change_files_for_nihr)
  stub(generate_email_alerts, "generate_all_pubmed_dfs_for_all_programs_and_registries", mock_generate_all_pubmed_dfs_for_all_programs_and_registries)
  stub(generate_email_alerts, "load_attachments_and_send_email_alert_for_all_programs", mock_load_attachments_and_send_email_alert_for_all_programs)
  stub(generate_email_alerts, "emayili::server", mock_smtp_server)
  stub(generate_email_alerts, "readr::read_file", function(...) "mock_password")
  stub(generate_email_alerts, "DBI::dbConnect", function(...) structure(list(), class = "DBIConnection"))
  stub(generate_email_alerts, "DBI::dbDisconnect", function(...) TRUE)

  # Call the function
  generate_email_alerts(FALSE, base_path = testthat::test_path("data/email_attachments"))

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})

test_that("generate_email_alerts creates daily directory if it does not exist", {
  # Mock the dependencies
  stub(generate_email_alerts, "generate_change_files_for_nct_isrctn_eu", mock_generate_change_files_for_nct_isrctn_eu)
  stub(generate_email_alerts, "generate_change_files_for_nihr", mock_generate_change_files_for_nihr)
  stub(generate_email_alerts, "generate_all_pubmed_dfs_for_all_programs_and_registries", mock_generate_all_pubmed_dfs_for_all_programs_and_registries)
  stub(generate_email_alerts, "load_attachments_and_send_email_alert_for_all_programs", mock_load_attachments_and_send_email_alert_for_all_programs)
  stub(generate_email_alerts, "emayili::server", mock_smtp_server)
  stub(generate_email_alerts, "readr::read_file", function(...) "mock_password")
  stub(generate_email_alerts, "DBI::dbConnect", function(...) structure(list(), class = "DBIConnection"))
  stub(generate_email_alerts, "DBI::dbDisconnect", function(...) TRUE)

  # Create a temporary directory path
  base_path <- testthat::test_path("data/email_attachments")
  daily_path <- file.path(base_path, Sys.Date(), "/")
  message("Testing directory creation at: ", daily_path)

  # Ensure the directory does not exist before the test
  if (dir.exists(daily_path)) {
    unlink(daily_path, recursive = TRUE)
  }

  # Call the function
  generate_email_alerts(FALSE, base_path = base_path)

  # Assertions
  expect_true(dir.exists(daily_path)) # The directory should be created
})
