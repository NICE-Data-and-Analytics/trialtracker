library(testthat)
library(compareDF)
library(mockery)

# Mock data for testing
Change_DF <- readRDS(testthat::test_path("test_data/example_Compare_DF_obj.rds"))
DF_Name <- "NCT"
prog_name <- "COVID"
daily_path <- testthat::test_path("test_data/write_changes-test")

# Unit tests
test_that("write_changes_to_disk function and arguments call", {
  # Mock the create_output_table function
  mock_create_output_table <- mock()
  stub(write_changes_to_disk, "compareDF::create_output_table", mock_create_output_table)

  # Call the function
  write_changes_to_disk(Change_DF, DF_Name, prog_name)

  # Check if create_output_table was called once
  expect_called(mock_create_output_table, 1)

  # Check the arguments passed to create_output_table
  expect_call(
    mock_create_output_table,
    1,
    compareDF::create_output_table(
      Change_DF,
      output_type = "xlsx",
      file_name = file.path(
        daily_path,
        paste0(
          DF_Name,
          "_",
          prog_name,
          "_Registry_Changes-",
          Sys.Date(),
          ".xlsx"
        )
      )
    )
  )
})

test_that("write_changes_to_disk writes file to disk", {
  # Create test directory
  if (!dir.exists(daily_path)) {
    dir.create(daily_path)
  }

  # Create full path variable
  full_path <- file.path(
    daily_path,
    paste0(
      DF_Name,
      "_",
      prog_name,
      "_Registry_Changes-",
      Sys.Date(),
      ".xlsx"
    )
  )

  # Call the function
  write_changes_to_disk(Change_DF, daily_path, DF_Name, prog_name)

  # Check if file exists
  expect_true(file.exists(full_path))

  # Clean up
  unlink(daily_path, recursive = TRUE, force = TRUE)
})
