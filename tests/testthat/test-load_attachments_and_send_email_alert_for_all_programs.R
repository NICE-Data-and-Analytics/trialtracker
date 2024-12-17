library(testthat)
library(mockery)

# Mock function
mock_load_attachments_and_send_email_alert_for_one_program <- function(program, daily_path, dev_flag, smtp) {
  message("Mock function called for program: ", program)
}

test_that("load_attachments_and_send_email_alert_for_all_programs works with multiple programs", {
  # Mock the dependency
  stub(load_attachments_and_send_email_alert_for_all_programs, "load_attachments_and_send_email_alert_for_one_program", mock_load_attachments_and_send_email_alert_for_one_program)

  # Mock inputs
  programs <- c("program1", "program2")
  daily_path <- "mock/path"
  dev_flag <- FALSE
  smtp <- NULL

  # Call the function
  load_attachments_and_send_email_alert_for_all_programs(programs, daily_path, dev_flag, smtp)

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})

test_that("load_attachments_and_send_email_alert_for_all_programs works with no programs", {
  # Mock the dependency
  stub(load_attachments_and_send_email_alert_for_all_programs, "load_attachments_and_send_email_alert_for_one_program", mock_load_attachments_and_send_email_alert_for_one_program)

  # Mock inputs
  programs <- character(0)
  daily_path <- "mock/path"
  dev_flag <- FALSE
  smtp <- NULL

  # Call the function
  load_attachments_and_send_email_alert_for_all_programs(programs, daily_path, dev_flag, smtp)

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})

test_that("load_attachments_and_send_email_alert_for_all_programs works with a single program", {
  # Mock the dependency
  stub(load_attachments_and_send_email_alert_for_all_programs, "load_attachments_and_send_email_alert_for_one_program", mock_load_attachments_and_send_email_alert_for_one_program)

  # Mock inputs
  programs <- c("program1")
  daily_path <- "mock/path"
  dev_flag <- FALSE
  smtp <- NULL

  # Call the function
  load_attachments_and_send_email_alert_for_all_programs(programs, daily_path, dev_flag, smtp)

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})
