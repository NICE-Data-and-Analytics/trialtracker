# Load necessary libraries
library(testthat)

# Define the test
test_that("generate_esearch_url generates the correct URL", {
  # Define test inputs
  search_term <- "cancer"
  api_key <- "123456789"
  mindate <- as.Date("2024-10-17")
  maxdate <- as.Date("2024-10-17")

  # Expected URL
  expected_url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed",
    "&mindate=", mindate,
    "&maxdate=", maxdate,
    "&datetype=edat&retmode=json&api_key=", api_key,
    "&term=", search_term
  )

  # Call the function
  generated_url <- generate_esearch_url(search_term, api_key, mindate, maxdate)

  # Check if the generated URL matches the expected URL
  expect_equal(generated_url, expected_url)
})

# Define the test for default dates
test_that("generate_esearch_url uses default dates correctly", {
  # Define test inputs
  search_term <- "diabetes"
  api_key <- "123456789"
  default_date <- Sys.Date() - 1

  # Expected URL
  expected_url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed",
    "&mindate=", default_date,
    "&maxdate=", default_date,
    "&datetype=edat&retmode=json&api_key=", api_key,
    "&term=", search_term
  )

  # Call the function with default dates
  generated_url <- generate_esearch_url(search_term, api_key)

  # Check if the generated URL matches the expected URL
  expect_equal(generated_url, expected_url)
})
