library(testthat)
library(mockery)
library(httr2)

# Mock functions
mock_generate_efetch_url <- function(pmid, api_object) {
  paste0("https://mockurl.com/efetch?db=pubmed&api_key=", api_object, "&id=", pmid)
}

mock_request <- function(url) {
  structure(list(url = url), class = "httr2_request")
}

mock_req_perform <- function(request) {
  structure(list(request = request), class = "httr2_response")
}

mock_resp_body_xml <- function(response) {
  list(result = "mock_result")
}

test_that("get_efetch_results works with valid inputs", {
  # Mock the dependencies
  stub(get_efetch_results, "generate_efetch_url", mock_generate_efetch_url)
  stub(get_efetch_results, "httr2::request", mock_request)
  stub(get_efetch_results, "httr2::req_perform", mock_req_perform)
  stub(get_efetch_results, "httr2::resp_body_xml", mock_resp_body_xml)

  # Call the function
  pmid <- "12345678"
  api_object <- "test_api_key"
  result <- get_efetch_results(pmid, api_object)

  # Assertions
  expect_equal(result, list(result = "mock_result"))
})
