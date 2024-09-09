library(testthat)
library(mockery)
library(readr)

# Mock data for testing
PM_DF <- data.frame(
  col1 = c("data1", "data2"),
  col2 = c("data3", "data4")
)
daily_path <- "tests/testthat/test_dir/"
PM_DF_Name <- "TestDF"
prog_name <- "TestProg"

# Unit tests
test_that("write_PM_dfs_to_disk writes CSV when PM_DF has rows", {
  # Mock the write_csv function
  mock_write_csv <- mock()
  stub(write_PM_dfs_to_disk, "write_csv", mock_write_csv)

  # Call the function
  write_PM_dfs_to_disk(PM_DF, daily_path, PM_DF_Name, prog_name)

  # Check if write_csv was called once
  expect_called(mock_write_csv, 1)

  # Check the arguments passed to write_csv
  expect_call(mock_write_csv, 1, write_csv(
    PM_DF,
    paste0(
      daily_path,
      prog_name,
      PM_DF_Name,
      "_Publications_",
      Sys.Date(),
      ".csv"
    )
  ))
})

test_that("write_PM_dfs_to_disk does not write CSV when PM_DF is empty", {
  # Mock the write_csv function
  mock_write_csv <- mock()
  stub(write_PM_dfs_to_disk, "write_csv", mock_write_csv)

  # Call the function with an empty data frame
  write_PM_dfs_to_disk(data.frame(), daily_path, PM_DF_Name, prog_name)

  # Check if write_csv was not called
  expect_called(mock_write_csv, 0)
})
