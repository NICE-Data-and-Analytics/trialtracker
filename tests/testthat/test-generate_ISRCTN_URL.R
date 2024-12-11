library(testthat)

test_that("generate_ISRCTN_URL works correctly", {
  # Test 1: Basic Functionality
  isrctn_ids1 <- c("ISRCTN10985036", "ISRCTN25778550")
  expected_url1 <- "http://www.isrctn.com/api/query/format/who?q=ISRCTN10985036%20OR%20ISRCTN25778550&limit=12"
  expect_equal(generate_ISRCTN_URL(isrctn_ids1), expected_url1)

  # Test 2: Empty Vector
  isrctn_ids2 <- character(0)
  expect_error(generate_ISRCTN_URL(isrctn_ids2), "The input vector is empty. Please provide at least one ISRCTN trial ID.")

  # Test 3: Single ID
  isrctn_ids4 <- c("ISRCTN10985036")
  expected_url4 <- "http://www.isrctn.com/api/query/format/who?q=ISRCTN10985036&limit=11"
  expect_equal(generate_ISRCTN_URL(isrctn_ids4), expected_url4)
})
