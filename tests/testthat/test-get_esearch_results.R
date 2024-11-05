# Load necessary libraries
library(testthat)
library(withr)
library(httptest2)

# Define the test
test_that("get_esearch_results retrieves the correct results", {
  # Define test inputs
  search_term <- "cancer"
  api_key <- "your_api_key"
  mindate <- as.Date("2024-10-17")
  maxdate <- as.Date("2024-10-17")

  # Define the expected response
  expected_response <- list(
    esearchresult = list(
      count = "1",
      idlist = list("12345678")
    )
  )

  # Capture the request and response if not already captured
  if (!dir.exists("mock_dir")) {
    dir.create("mock_dir")
    withr::with_dir("mock_dir", {
      capture_requests({
        get_esearch_results(search_term, api_key, mindate, maxdate)
      })
    })
  }

  # Use the captured response
  with_mock_dir("mock_dir", {
    # Call the function
    results <- get_esearch_results(search_term, api_key, mindate, maxdate)

    # Check if the results match the expected response
    expect_equal(results, expected_response)
  })
})

# Define the test for default dates
test_that("get_esearch_results uses default dates correctly", {
  # Define test inputs
  search_term <- "diabetes"
  api_key <- "your_api_key"
  default_date <- Sys.Date() - 1

  # Define the expected response
  expected_response <- list(
    esearchresult = list(
      count = "1",
      idlist = list("12345678")
    )
  )

  # Capture the request and response if not already captured
  if (!dir.exists("mock_dir")) {
    dir.create("mock_dir")
    withr::with_dir("mock_dir", {
      capture_requests({
        get_esearch_results(search_term, api_key)
      })
    })
  }

  # Use the captured response
  with_mock_dir("mock_dir", {
    # Call the function with default dates
    results <- get_esearch_results(search_term, api_key)

    # Check if the results match the expected response
    expect_equal(results, expected_response)
  })
})
