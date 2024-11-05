# Load necessary libraries
library(testthat)
library(withr)
library(httptest2)

# Define the test
test_that("get_esearch_results retrieves the correct results", {
  # Define test inputs
  proj_root <- rprojroot::find_root(rprojroot::is_r_package)
  setwd(proj_root)
  api <- readr::read_file("secrets/entrez.key")
  search_term <- "cancer"
  mindate <- as.Date("2024-10-17")
  maxdate <- as.Date("2024-10-17")

  # Define the expected response
  expected_response <- list(
    header = list(type = "esearch", version = "0.3"),
    esearchresult = list(count = "725",
                         retmax = "20",
                         retstart = "0",
                         idlist = list("39418133",
                                       "39418094",
                                       "39418072",
                                       "39418061",
                                       "39418058",
                                       "39418052",
                                       "39418050",
                                       "39418046",
                                       "39418043",
                                       "39418037",
                                       "39418029",
                                       "39418020",
                                       "39418017",
                                       "39417994",
                                       "39417984",
                                       "39417979",
                                       "39417978",
                                       "39417976",
                                       "39417968",
                                       "39417961"),
                         translationset = list(list(from = "cancer",
                                               to = "\"cancer's\"[All Fields] OR \"cancerated\"[All Fields] OR \"canceration\"[All Fields] OR \"cancerization\"[All Fields] OR \"cancerized\"[All Fields] OR \"cancerous\"[All Fields] OR \"neoplasms\"[MeSH Terms] OR \"neoplasms\"[All Fields] OR \"cancer\"[All Fields] OR \"cancers\"[All Fields]")),
                         querytranslation = "(\"cancer s\"[All Fields] OR \"cancerated\"[All Fields] OR \"canceration\"[All Fields] OR \"cancerization\"[All Fields] OR \"cancerized\"[All Fields] OR \"cancerous\"[All Fields] OR \"neoplasms\"[MeSH Terms] OR \"neoplasms\"[All Fields] OR \"cancer\"[All Fields] OR \"cancers\"[All Fields]) AND 2024/10/17[Date - Entry]")
    )

    # Call the function
    results <- get_esearch_results(search_term, api, mindate, maxdate)

    # Check if the results match the expected response
    expect_equal(results, expected_response)

})
