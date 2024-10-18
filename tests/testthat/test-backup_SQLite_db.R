# Load necessary libraries
library(testthat)
library(withr)

# Define the test
test_that("backup_SQLite_db creates the correct directories and copies files", {
  # Create a temporary directory to act as the data directory
  temp_data_dir <- local_tempdir()

  # Create a mock RSQLite_Data directory with some dummy files
  rsqlite_data_dir <- file.path(temp_data_dir, "RSQLite_data")
  dir.create(rsqlite_data_dir, recursive = TRUE)
  file.create(file.path(rsqlite_data_dir, "dummy1.sqlite"))
  file.create(file.path(rsqlite_data_dir, "dummy2.sqlite"))

  # Mock the data directory path
  withr::local_dir(temp_data_dir)

  # Call the function
  backup_SQLite_db()

  # Check if the monthly backup directory was created
  monthly_backup_dir <- file.path(temp_data_dir, "monthly_SQLite_backup", stringr::str_sub(as.character(Sys.Date()), 1, 7))
  expect_true(dir.exists(monthly_backup_dir))

  # Check if the files were copied to the monthly backup directory
  copied_files <- dir(monthly_backup_dir, full.names = TRUE)
  expect_equal(length(copied_files), 2)
  expect_true(file.exists(file.path(monthly_backup_dir, "dummy1.sqlite")))
  expect_true(file.exists(file.path(monthly_backup_dir, "dummy2.sqlite")))
})
