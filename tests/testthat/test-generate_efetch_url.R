library(testthat)

test_that("generate_efetch_url works with valid inputs", {
  pmid <- "12345678"
  api_object <- "test_api_key"
  expected_url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed",
    "&api_key=", api_object,
    "&id=", pmid
  )

  result <- generate_efetch_url(pmid, api_object)
  expect_equal(result, expected_url)
})
