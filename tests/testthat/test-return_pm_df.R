library(testthat)
library(mockery)

test_that("return_pm_df works with valid inputs", {
  # Mock functions
  mock_xml_text <- function(...) "mock_text"
  mock_xml_find_first <- function(...) structure(list(), class = "xml_node")
  mock_xml_find_all <- function(...) structure(list(), class = "xml_nodeset")

  # Use mockery to temporarily replace functions
  stub(return_pm_df, "xml2::xml_text", mock_xml_text)
  stub(return_pm_df, "xml2::xml_find_first", mock_xml_find_first)
  stub(return_pm_df, "xml2::xml_find_all", mock_xml_find_all)

  # Call the function
  single_id_query_results <- structure(list(), class = "xml_document")
  search_term <- "cancer"
  result <- return_pm_df(single_id_query_results, search_term)

  # Expected result with fixed date
  fixed_date <- as.Date("2024-11-06")
  expected_result <- tibble::tibble(
    "ID" = search_term,
    "Query_Date" = fixed_date,
    "pmid" = "mock_text",
    "doi" = "https://doi.org/mock_text",
    "title" = "mock_text",
    "abstract" = "mock_text",
    "jabbrv" = "mock_text",
    "journal" = "mock_text"
  )

  # Adjust the result to match the fixed date
  result$Query_Date <- fixed_date

  # Assertions
  expect_equal(result, expected_result)
})
