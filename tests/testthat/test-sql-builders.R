testthat::test_that("latest SQL builders return character SQL with expected structure", {
  con <- create_sqlite_con()

  sql_nct <- nct_latest_sql_from_trial_ids(con)
  sql_eu <- eu_latest_sql_from_trial_ids(con)
  sql_isr <- isr_latest_sql_from_trial_ids(con)
  sql_nihr <- nihr_latest_sql_from_trial_ids(con)

  for (sql in list(sql_nct, sql_eu, sql_isr, sql_nihr)) {
    testthat::expect_type(sql, "character")
    testthat::expect_match(sql, "WITH\\s+\\s*ids\\s+AS", perl = TRUE)
    testthat::expect_match(sql, "latest_rank", fixed = TRUE)
    testthat::expect_match(sql, "ROW_NUMBER\\(\\) OVER", perl = TRUE)
    testthat::expect_match(sql, "first_seen", fixed = TRUE)
    testthat::expect_match(sql, "ORDER BY", fixed = TRUE)
  }

  # Registry-specific invariants
  testthat::expect_match(sql_nct, 'TRIM("NCT_Ids")', fixed = TRUE)
  testthat::expect_match(sql_nct, 'FROM "NCT"', fixed = TRUE)
  testthat::expect_match(sql_nct, 'USING ("NCTId")', fixed = TRUE)

  testthat::expect_match(sql_eu, 'TRIM("EU_Ids")', fixed = TRUE)
  testthat::expect_match(sql_eu, 'FROM "EU"', fixed = TRUE)
  testthat::expect_match(sql_eu, 'USING ("EU_Ids")', fixed = TRUE)

  testthat::expect_match(sql_isr, 'TRIM("ISRCTN_Ids")', fixed = TRUE)
  testthat::expect_match(sql_isr, 'FROM "ISRCTN"', fixed = TRUE)
  testthat::expect_match(sql_isr, 'USING ("ISRCTN_No")', fixed = TRUE)

  # NIHR normalisation appears in ids and join
  testthat::expect_match(
    sql_nihr,
    'REPLACE\\(REPLACE\\(TRIM\\("NIHR_Ids"\\)',
    perl = TRUE
  )
  testthat::expect_match(
    sql_nihr,
    'REPLACE\\(REPLACE\\(n\\."project_id"',
    perl = TRUE
  )
})
