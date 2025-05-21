library(testthat)
loadNamespace("tibble")
library(mockery)

test_that("generate_NCT_DF works correctly", {
  # Mock API response for testing
  mock_response <- list(studies = list(list(
    protocolSection = list(
      identificationModule = list(
        nctId = "NCT03146143",
        orgStudyIdInfo = list(id = "ORG123"),
        briefTitle = "Study Title 1",
        acronym = "ST1"
      ),
      conditionsModule = list(conditions = c("Condition1", "Condition2")),
      statusModule = list(
        overallStatus = "Recruiting",
        primaryCompletionDateStruct = list(date = "2023-01-01"),
        completionDateStruct = list(date = "2023-06-01"),
        resultsFirstSubmitDate = "2023-02-01",
        resultsFirstPostDateStruct = list(date = "2023-03-01"),
        lastUpdatePostDateStruct = list(date = "2023-04-01")
      ),
      referencesModule = list(seeAlsoLinks = list(list(url = "http://example.com")))
    )
  )))

  # Mock the read_json function to return the mock response
  stub(generate_NCT_DF, "jsonlite::read_json", function(...)
    mock_response)

  # Test 1: Basic Functionality
  api_url <- "https://clinicaltrials.gov/api/v2/studies?format=json&filter.ids=NCT03146143&fields=NCTId%7COrgStudyId%7CCondition%7CBriefTitle%7CAcronym%7COverallStatus%7CPrimaryCompletionDate%7CCompletionDate%7CResultsFirstSubmitDate%7CResultsFirstPostDate%7CLastUpdatePostDate%7CSeeAlsoLinkURL&pageSize=1000"
  df <- generate_NCT_DF(api_url)
  expected_df <- tibble::tibble(
    Rank = 1,
    NCTId = "NCT03146143",
    OrgStudyId = "ORG123",
    Condition = "Condition1|Condition2",
    BriefTitle = "Study Title 1",
    Acronym = "ST1",
    OverallStatus = "Recruiting",
    PrimaryCompletionDate = "2023-01-01",
    CompletionDate = "2023-06-01",
    ResultsFirstSubmitDate = "2023-02-01",
    ResultsFirstPostDate = "2023-03-01",
    LastUpdatePostDate = "2023-04-01",
    SeeAlsoLinkURL = "http://example.com"
  )
  expect_equal(df, expected_df)

  # Test 2: Empty Response
  mock_response_empty <- list(studies = list())
  stub(generate_NCT_DF, "jsonlite::read_json", function(...)
    mock_response_empty)
  df_empty <- generate_NCT_DF(api_url)
  expect_equal(
    df_empty,
    tibble::tibble(
      Rank = integer(),
      NCTId = character(),
      OrgStudyId = character(),
      Condition = character(),
      BriefTitle = character(),
      Acronym = character(),
      OverallStatus = character(),
      PrimaryCompletionDate = character(),
      CompletionDate = character(),
      ResultsFirstSubmitDate = character(),
      ResultsFirstPostDate = character(),
      LastUpdatePostDate = character(),
      SeeAlsoLinkURL = character()
    )
  )

  # Test 3: Missing Fields
  mock_response_missing_fields <- list(studies = list(list(
    protocolSection = list(identificationModule = list(nctId = "NCT03146143"))
  )))
  stub(generate_NCT_DF, "jsonlite::read_json", function(...)
    mock_response_missing_fields)
  df_missing_fields <- generate_NCT_DF(api_url)
  expected_df_missing_fields <- tibble::tibble(
    Rank = 1,
    NCTId = "NCT03146143",
    OrgStudyId = NA_character_,
    Condition = NA_character_,
    BriefTitle = NA_character_,
    Acronym = NA_character_,
    OverallStatus = NA_character_,
    PrimaryCompletionDate = NA_character_,
    CompletionDate = NA_character_,
    ResultsFirstSubmitDate = NA_character_,
    ResultsFirstPostDate = NA_character_,
    LastUpdatePostDate = NA_character_,
    SeeAlsoLinkURL = NA_character_
  )
  expect_equal(df_missing_fields, expected_df_missing_fields)


  # Test 4: Invalid URL returns empty tibble with correct structure
  stub(generate_NCT_DF, "jsonlite::read_json", function(...)
    stop("Invalid URL"))
  df_invalid <- generate_NCT_DF("invalid_url")

  # Check that the result is a tibble with 0 rows
  expect_s3_class(df_invalid, "tbl_df")
  expect_equal(nrow(df_invalid), 0)

  # Check that all expected columns are present
  expected_columns <- c(
    "Rank",
    "NCTId",
    "OrgStudyId",
    "Condition",
    "BriefTitle",
    "Acronym",
    "OverallStatus",
    "PrimaryCompletionDate",
    "CompletionDate",
    "ResultsFirstSubmitDate",
    "ResultsFirstPostDate",
    "LastUpdatePostDate",
    "SeeAlsoLinkURL"
  )

})
