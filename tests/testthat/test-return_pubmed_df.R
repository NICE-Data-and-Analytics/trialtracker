library(testthat)
library(dplyr)
library(stringr)
library(DBI)

# Sample data for testing
sample_data <- data.frame(
  Query_Date = as.Date(c("2023-01-01", "2023-02-01", "2023-02-01", "2023-02-01", "2023-03-05", "2023-05-01", "2023-06-01", "2023-05-05")),
  Guideline.number = c("GL123", "GL123", "GL456", "IP34", "GL789", "GL123", "GL456", "GL789"),
  doi = c("10.1000/xyz123", "10.1000/xyz123", "10.1000/xyz456", "10.1000/ert453", "10.1000/xyz123", "10.1000/xyz123", "10.1000/xyz456", "10.1000/xyz123")
)

# Mock database connection and table
con <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWriteTable(con, "registry_table", sample_data)

# Unit test
test_that("return_pubmed_df works correctly", {
  result <- return_pubmed_df(con, "registry_table", as.Date("2023-01-01"), as.Date("2023-03-01"), "GL")

  # Check the structure of the result
  expect_s3_class(result, "data.frame")

  # Check the number of rows (distinct DOIs)
  expect_equal(nrow(result), 2)

  # Check the content of the result
  expect_equal(result$doi, c("10.1000/xyz123", "10.1000/xyz456"))
  expect_equal(result$Query_Date, as.Date(c("2023-01-01", "2023-02-01")))
})

# Clean up
dbDisconnect(con)
