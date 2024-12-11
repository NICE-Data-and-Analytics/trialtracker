library(testthat)
library(tibble)
library(dplyr)
library(purrr)
library(mockery)

test_that("generate_pm_tibble_from_search_term returns expected tibble", {
  # Mock functions
  mock_get_esearch_results <- function(search_term, api_object, mindate, maxdate) {
    if (search_term == "") {
      return(list(esearchresult = list(idlist = character(0))))
    }
    list(
      esearchresult = list(
        idlist = c("12345678", "87654321")
      )
    )
  }

  mock_get_efetch_results_in_tibble_form <- function(pmid, api_object, search_term) {
    tibble::tibble(
      pmid = pmid,
      title = paste("Title for", pmid),
      abstract = paste("Abstract for", pmid),
      search_term = search_term
    )
  }

  # Use mockery to temporarily replace functions
  stub(trialtracker::generate_pm_tibble_from_search_term, "get_esearch_results", mock_get_esearch_results)
  stub(trialtracker::generate_pm_tibble_from_search_term, "get_efetch_results_in_tibble_form", mock_get_efetch_results_in_tibble_form)

  search_term <- "cancer"
  api_object <- "fake_api_key"
  mindate <- Sys.Date() - 1
  maxdate <- Sys.Date() - 1

  result <- trialtracker::generate_pm_tibble_from_search_term(search_term, api_object, mindate, maxdate)

  #print(result)  # Debugging statement

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$pmid, c("12345678", "87654321"))
  expect_equal(result$title, c("Title for 12345678", "Title for 87654321"))
  expect_equal(result$abstract, c("Abstract for 12345678", "Abstract for 87654321"))
  expect_equal(result$search_term, rep(search_term, 2))

})

test_that("generate_pm_tibble_from_search_term handles invalid input gracefully", {
  # Mock functions
  mock_get_esearch_results <- function(search_term, api_object, mindate, maxdate) {
    if (search_term == "") {
      return(list(esearchresult = list(idlist = character(0))))
    }
    list(
      esearchresult = list(
        idlist = c("12345678", "87654321")
      )
    )
  }

  mock_get_efetch_results_in_tibble_form <- function(pmid, api_object, search_term) {
    tibble::tibble(
      pmid = pmid,
      title = paste("Title for", pmid),
      abstract = paste("Abstract for", pmid),
      search_term = search_term
    )
  }

  # Use mockery to temporarily replace functions
  stub(trialtracker::generate_pm_tibble_from_search_term, "get_esearch_results", mock_get_esearch_results)
  stub(trialtracker::generate_pm_tibble_from_search_term, "get_efetch_results_in_tibble_form", mock_get_efetch_results_in_tibble_form)

  search_term <- ""
  api_object <- "fake_api_key"
  mindate <- Sys.Date() - 1
  maxdate <- Sys.Date() - 1

  result <- trialtracker::generate_pm_tibble_from_search_term(search_term, api_object, mindate, maxdate)

  #print(result)  # Debugging statement

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) == 0)
})
