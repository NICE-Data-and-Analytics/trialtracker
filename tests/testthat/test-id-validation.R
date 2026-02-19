testthat::test_that("registry_key_from_col maps known columns and errors otherwise", {
  testthat::expect_identical(registry_key_from_col("NCT_Ids"), "NCT")
  testthat::expect_identical(registry_key_from_col("EU_Ids"), "EU")
  testthat::expect_identical(registry_key_from_col("ISRCTN_Ids"), "ISRCTN")
  testthat::expect_identical(registry_key_from_col("NIHR_Ids"), "NIHR")
  testthat::expect_error(
    registry_key_from_col("NOPE"),
    "Unknown registry_col",
    fixed = TRUE
  )
})

testthat::test_that("normalize_trial_id trims and normalises NIHR only", {
  testthat::expect_identical(
    normalize_trial_id("  NCT00000001 ", "NCT"),
    "NCT00000001"
  )
  testthat::expect_identical(
    normalize_trial_id(" RP-PG-0610/1010 ", "NIHR"),
    "RPPG06101010"
  )
  testthat::expect_identical(
    normalize_trial_id(" RP-PG-0610/1010 ", "NIHR_Ids"),
    "RPPG06101010"
  )
})

testthat::test_that("is_valid_trial_id validates against specific registry and any registry", {
  testthat::expect_true(is_valid_trial_id("NCT12345678", "NCT"))
  testthat::expect_false(is_valid_trial_id("NCT123", "NCT"))

  testthat::expect_true(is_valid_trial_id("ISRCTN12345678", "ISRCTN_Ids"))
  testthat::expect_false(is_valid_trial_id("ISRCTN123", "ISRCTN"))

  testthat::expect_true(is_valid_trial_id("2020-000001-01", "EU"))
  testthat::expect_false(is_valid_trial_id("2020-1-01", "EU"))

  # any registry
  testthat::expect_true(is_valid_trial_id("NCT12345678"))
  testthat::expect_true(is_valid_trial_id("ISRCTN12345678"))
  testthat::expect_true(is_valid_trial_id("2020-000001-01"))
  testthat::expect_false(is_valid_trial_id("totallybogus"))
})

testthat::test_that("invalid_upload_rows returns indices with any invalid non-empty IDs", {
  df <- data.frame(
    NCT_Ids = c("NCT12345678", "NCT123", ""),
    EU_Ids = c("", "2020-000001-01", "bad"),
    stringsAsFactors = FALSE
  )
  bad <- invalid_upload_rows(df)
  testthat::expect_identical(bad, c(2L, 3L))
})

testthat::test_that("invalid_upload_rows returns integer(0) for empty input or no matching cols", {
  testthat::expect_identical(invalid_upload_rows(NULL), integer())
  testthat::expect_identical(invalid_upload_rows(data.frame()), integer())
  testthat::expect_identical(
    invalid_upload_rows(data.frame(x = 1:2)),
    integer()
  )
})
