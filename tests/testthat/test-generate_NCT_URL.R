library(testthat)

test_that("generate_NCT_URL works correctly", {
  # Test 1: Basic Functionality
  trial_ids1 <- c("NCT03146143", "NCT00408044", "NCT01377987")
  expected_url1 <- paste0(
    "https://clinicaltrials.gov/api/v2/studies?format=json&filter.ids=",
    "NCT03146143%7CNCT00408044%7CNCT01377987",
    "&fields=",
    paste("NCTId", "OrgStudyId", "Condition",
          "BriefTitle", "Acronym",
          "OverallStatus", "PrimaryCompletionDate",
          "CompletionDate", "ResultsFirstSubmitDate", "ResultsFirstPostDate",
          "LastUpdatePostDate", "SeeAlsoLinkURL",
          sep = "%7C"),
    "&pageSize=1000"
  )
  expect_equal(generate_NCT_URL(trial_ids1), expected_url1)

  # Test 2: Empty Vector
  trial_ids2 <- character(0)
  expected_url2 <- paste0(
    "https://clinicaltrials.gov/api/v2/studies?format=json&filter.ids=",
    "",
    "&fields=",
    paste("NCTId", "OrgStudyId", "Condition",
          "BriefTitle", "Acronym",
          "OverallStatus", "PrimaryCompletionDate",
          "CompletionDate", "ResultsFirstSubmitDate", "ResultsFirstPostDate",
          "LastUpdatePostDate", "SeeAlsoLinkURL",
          sep = "%7C"),
    "&pageSize=1000"
  )
  expect_equal(generate_NCT_URL(trial_ids2), expected_url2)

  # Test 3: Vector with NAs
  trial_ids3 <- c("NCT03146143", NA, "NCT01377987")
  expected_url3 <- paste0(
    "https://clinicaltrials.gov/api/v2/studies?format=json&filter.ids=",
    "NCT03146143%7CNCT01377987",
    "&fields=",
    paste("NCTId", "OrgStudyId", "Condition",
          "BriefTitle", "Acronym",
          "OverallStatus", "PrimaryCompletionDate",
          "CompletionDate", "ResultsFirstSubmitDate", "ResultsFirstPostDate",
          "LastUpdatePostDate", "SeeAlsoLinkURL",
          sep = "%7C"),
    "&pageSize=1000"
  )
  expect_equal(generate_NCT_URL(trial_ids3), expected_url3)

  # Test 4: Vector with Duplicates
  trial_ids4 <- c("NCT03146143", "NCT03146143", "NCT01377987")
  expected_url4 <- paste0(
    "https://clinicaltrials.gov/api/v2/studies?format=json&filter.ids=",
    "NCT03146143%7CNCT01377987",
    "&fields=",
    paste("NCTId", "OrgStudyId", "Condition",
          "BriefTitle", "Acronym",
          "OverallStatus", "PrimaryCompletionDate",
          "CompletionDate", "ResultsFirstSubmitDate", "ResultsFirstPostDate",
          "LastUpdatePostDate", "SeeAlsoLinkURL",
          sep = "%7C"),
    "&pageSize=1000"
  )
  expect_equal(generate_NCT_URL(trial_ids4), expected_url4)

  # Test 5: Combination of NAs and Duplicates
  trial_ids5 <- c("NCT03146143", NA, "NCT03146143", "NCT01377987")
  expected_url5 <- paste0(
    "https://clinicaltrials.gov/api/v2/studies?format=json&filter.ids=",
    "NCT03146143%7CNCT01377987",
    "&fields=",
    paste("NCTId", "OrgStudyId", "Condition",
          "BriefTitle", "Acronym",
          "OverallStatus", "PrimaryCompletionDate",
          "CompletionDate", "ResultsFirstSubmitDate", "ResultsFirstPostDate",
          "LastUpdatePostDate", "SeeAlsoLinkURL",
          sep = "%7C"),
    "&pageSize=1000"
  )
  expect_equal(generate_NCT_URL(trial_ids5), expected_url5)
})
