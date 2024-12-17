library(testthat)
library(mockery)
loadNamespace("readr")

# Mock data for testing
PM_DF <- data.frame(
  col1 = c("data1", "data2"),
  col2 = c("data3", "data4")
)
daily_path <- testthat::test_path("test_write_pubmed_to_disk_dir/")
PM_DF_Name <- "TestDF"

# Ensure the directory exists
if (!dir.exists(daily_path)) {
  dir.create(daily_path, recursive = TRUE)
}

# Unit tests
test_that("write_pubmed_dfs_to_disk writes CSV when PM_DF has rows", {
  # Mock the write_csv function
  mock_write_csv <- mock()
  stub(write_pubmed_dfs_to_disk, "readr::write_csv", mock_write_csv)

  # Call the function
  write_pubmed_dfs_to_disk(PM_DF, daily_path, PM_DF_Name)

  # Check if write_csv was called once
  expect_called(mock_write_csv, 1)

  # Check the arguments passed to write_csv
  expect_call(mock_write_csv, 1, readr::write_csv(PM_DF, file.path(
    daily_path,
    paste0(PM_DF_Name, "_Publications_", Sys.Date(), ".csv")
  )))
})

test_that("write_pubmed_dfs_to_disk does not write CSV when PM_DF is empty", {
  # Mock the write_csv function
  mock_write_csv <- mock()
  stub(write_pubmed_dfs_to_disk, "write_csv", mock_write_csv)

  # Call the function with an empty data frame
  write_pubmed_dfs_to_disk(data.frame(), daily_path, PM_DF_Name)

  # Check if write_csv was not called
  expect_called(mock_write_csv, 0)
})

# Clean up: Remove the directory after tests
unlink(daily_path, recursive = TRUE)
