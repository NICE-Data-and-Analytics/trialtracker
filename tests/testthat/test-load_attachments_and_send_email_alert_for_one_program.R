library(testthat)
library(mockery)
loadNamespace("emayili")

# Mock functions
mock_generate_tt_email <- function(program, attachments, dev_flag) {
  emayili::envelope()
}

mock_smtp <- function(email) {
  message("Mock SMTP called")
  TRUE
}

test_that("load_attachments_and_send_email_alert_for_one_program works with valid inputs", {
  # Mock the dependencies
  stub(load_attachments_and_send_email_alert_for_one_program, "generate_tt_email", mock_generate_tt_email)
  stub(load_attachments_and_send_email_alert_for_one_program, "smtp", mock_smtp)

  # Mock inputs
  program <- "test_program"
  daily_path <- tempdir()
  dev_flag <- FALSE

  # Create mock files
  file.create(file.path(daily_path, "file_test_program_1.txt"))
  file.create(file.path(daily_path, "file_test_program_2.txt"))

  # Call the function
  load_attachments_and_send_email_alert_for_one_program(program, daily_path, dev_flag, smtp = mock_smtp)

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})

test_that("load_attachments_and_send_email_alert_for_one_program handles no matching files", {
  # Mock the dependencies
  stub(load_attachments_and_send_email_alert_for_one_program, "generate_tt_email", mock_generate_tt_email)
  stub(load_attachments_and_send_email_alert_for_one_program, "smtp", mock_smtp)

  # Mock inputs
  program <- "test_program"
  daily_path <- tempdir()
  dev_flag <- FALSE

  # Ensure no matching files
  file.create(file.path(daily_path, "file_other_program_1.txt"))

  # Call the function
  load_attachments_and_send_email_alert_for_one_program(program, daily_path, dev_flag, smtp = mock_smtp)

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})

test_that("load_attachments_and_send_email_alert_for_one_program works with dev_flag set to TRUE", {
  # Mock the dependencies
  stub(load_attachments_and_send_email_alert_for_one_program, "generate_tt_email", mock_generate_tt_email)
  stub(load_attachments_and_send_email_alert_for_one_program, "smtp", mock_smtp)

  # Mock inputs
  program <- "test_program"
  daily_path <- tempdir()
  dev_flag <- TRUE

  # Create mock files
  file.create(file.path(daily_path, "file_test_program_1.txt"))
  file.create(file.path(daily_path, "file_test_program_2.txt"))

  # Call the function
  load_attachments_and_send_email_alert_for_one_program(program, daily_path, dev_flag, smtp = mock_smtp)

  # Assertions
  expect_true(TRUE) # If no error is thrown, the test passes
})
