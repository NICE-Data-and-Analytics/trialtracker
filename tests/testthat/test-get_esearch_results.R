# Load necessary libraries
library(testthat)
loadNamespace("withr")

# Define the test
test_that("get_esearch_results retrieves the correct results", {

  # Set the environment variable locally for the test
  withr::local_envvar(ENTREZ_KEY_PATH = "c:/RStudio_Projects/Trialtracker/secrets/entrez.key")

    # Define test inputs
    api <- readr::read_file(Sys.getenv("ENTREZ_KEY_PATH"))
    search_term <- "cancer"
    mindate <- as.Date("2024-10-17")
    maxdate <- as.Date("2024-10-17")

    # Define the expected response
    expected_response <- list(
      header = list(type = "esearch", version = "0.3"),
      esearchresult = list(retmax = "20",
                           retstart = "0",
                           translationset = list(list(from = "cancer",
                                                      to = "\"cancer's\"[All Fields] OR \"cancerated\"[All Fields] OR \"canceration\"[All Fields] OR \"cancerization\"[All Fields] OR \"cancerized\"[All Fields] OR \"cancerous\"[All Fields] OR \"neoplasms\"[MeSH Terms] OR \"neoplasms\"[All Fields] OR \"cancer\"[All Fields] OR \"cancers\"[All Fields]")),
                           querytranslation = "(\"cancer s\"[All Fields] OR \"cancerated\"[All Fields] OR \"canceration\"[All Fields] OR \"cancerization\"[All Fields] OR \"cancerized\"[All Fields] OR \"cancerous\"[All Fields] OR \"neoplasms\"[MeSH Terms] OR \"neoplasms\"[All Fields] OR \"cancer\"[All Fields] OR \"cancers\"[All Fields]) AND 2024/10/17[Date - Entry]")
    )

    # Call the function
    results <- get_esearch_results(search_term, api, mindate, maxdate)

    # Remove 'count' and idlist from the actual results for comparison
    results_without_count <- results
    results_without_count$esearchresult$count <- NULL
    results_without_count$esearchresult$idlist <- NULL

    # Check if the results match the expected response
    expect_equal(results_without_count, expected_response)

})
