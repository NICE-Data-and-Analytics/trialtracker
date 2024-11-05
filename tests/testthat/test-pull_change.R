library(testthat)
library(dplyr)
library(compareDF)
library(mockery)

# Unit test for pull_change function
test_that("pull_change returns correct comparison", {
  # Mock the dbReadTable function
  mock_dbReadTable <- mock(
    data.frame(
      Query_Date = as.Date(c("2023-01-01", "2023-01-01")),
      Program = c("IP", "COVID"),
      Guideline.number = c("IP1934", "NG191"),
      URL = c("https://clinicaltrials.gov/ct2/show/NCT03321552", "https://clinicaltrials.gov/ct2/show/NCT04381936"),
      OrgStudyId = c("EU PMS Revision 4.1", "NDPHRECOVERY"),
      Condition = c("Critical Limb Ischemia", "Severe Acute Respiratory Syndrome"),
      BriefTitle = c("PROMISE International", "Randomised Evaluation of COVID-19 Therapy"),
      Acronym = c("<NA>", "RECOVERY"),
      OverallStatus = c("not recruiting", "Recruiting"),
      PrimaryCompletionDate = c("2024-01-24", "2026-06-30"),
      CompletionDate = c("2025-01-24", "2036-06-30"),
      ResultsFirstSubmitDate = c("<NA>", "<NA>"),
      ResultsFirstPostDate = c("<NA>", "<NA>"),
      LastUpdatePostDate = c("2024-01-19", "2024-01-19"),
      SeeAlsoLinkURL = c("", "https://www.recoverytrial.net/"),
      Comments = c("<NA>", "<NA>")
    ),
    data.frame(
      Query_Date = as.Date(c("2023-01-02", "2023-01-02")),
      Program = c("IP", "COVID"),
      Guideline.number = c("IP1934", "NG191"),
      URL = c("https://clinicaltrials.gov/ct2/show/NCT03321552", "https://clinicaltrials.gov/ct2/show/NCT04381936"),
      OrgStudyId = c("EU PMS Revision 4.1", "NDPHRECOVERY"),
      Condition = c("Critical Limb Ischemia", "Severe Acute Respiratory Syndrome"),
      BriefTitle = c("PROMISE International", "Randomised Evaluation of COVID-19 Therapy"),
      Acronym = c("<NA>", "RECOVERY"),
      OverallStatus = c("Recruiting", "Recruiting"),
      PrimaryCompletionDate = c("2024-01-25", "2026-06-30"),
      CompletionDate = c("2025-01-24", "2036-06-31"),
      ResultsFirstSubmitDate = c("<NA>", "<NA>"),
      ResultsFirstPostDate = c("<NA>", "<NA>"),
      LastUpdatePostDate = c("2024-01-20", "2024-01-23"),
      SeeAlsoLinkURL = c("", "https://www.recoverytrial.net/"),
      Comments = c("<NA>", "<NA>")
    )
  )

  # Mock the database connection
  con <- mock()

  # Replace dbReadTable with the mock
  stub(pull_change, "DBI::dbReadTable", mock_dbReadTable)

  # Call the function
  result <- pull_change(
    registry_table = "NCT",
    main_con = con,
    start_date = as.Date("2023-01-01"),
    end_date = as.Date("2023-01-02"),
    group_cols = "Guideline.number",
    exclude_cols = c(
      "Query_Date",
      "Comments",
      "Condition",
      "BriefTitle",
      "Acronym",
      "PrimaryCompletionDate",
      "ResultsFirstSubmitDate",
      "SeeAlsoLinkURL"
    ),
    regex_pattern = "IP[0-9]"
  )

  # Check the result
  expect_type(result, "list")
  expect_s3_class(result$comparison_df, "data.frame")
  expect_equal(nrow(result$comparison_df), 2)
  expect_equal(result$comparison_df$LastUpdatePostDate[1], "2024-01-20")
  expect_equal(result$comparison_df$chng_type[1], "+")
  expect_equal(result$comparison_df$OverallStatus[1], "Recruiting")
  expect_equal(result$comparison_df$LastUpdatePostDate[2], "2024-01-19")
  expect_equal(result$comparison_df$chng_type[2], "-")
  expect_equal(result$comparison_df$OverallStatus[2], "not recruiting")
})
