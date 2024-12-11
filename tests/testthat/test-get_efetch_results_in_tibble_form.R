library(testthat)
library(tibble)

# Mocking the get_efetch_results function
mock_get_efetch_results <- function(pmid, api_object) {
  tibble::tibble(
    pmid = pmid,
    title = "Sample Title",
    abstract = "Sample Abstract"
  )
}

# Mocking the return_pm_df function
mock_return_pm_df <- function(df, search_term) {
  df$search_term <- search_term
  return(df)
}

# Replacing the actual functions with mocks
assignInNamespace("get_efetch_results", mock_get_efetch_results, ns = "trialtracker")
assignInNamespace("return_pm_df", mock_return_pm_df, ns = "trialtracker")

test_that("get_efetch_results_in_tibble_form returns expected tibble", {
  pmid <- "12345678"
  api_object <- "fake_api_key"
  search_term <- "cancer"

  result <- get_efetch_results_in_tibble_form(pmid, api_object, search_term)

  expect_s3_class(result, "tbl_df")
  expect_equal(result$pmid, pmid)
  expect_equal(result$title, "Sample Title")
  expect_equal(result$abstract, "Sample Abstract")
  expect_equal(result$search_term, search_term)
})
