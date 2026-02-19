testthat::test_that("pubmed_sql maps reg to correct Trial_Ids column and PM table", {
  con <- create_sqlite_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  sql_nct <- pubmed_sql(con, "nct")
  testthat::expect_match(sql_nct, 'TRIM\\([`"]NCT_Ids[`"]\\)', perl = TRUE)
  testthat::expect_match(sql_nct, 'FROM\\s+[`"]NCT_PM[`"]\\s+p', perl = TRUE)

  sql_isr <- pubmed_sql(con, "ISRCTN")
  testthat::expect_match(sql_isr, 'TRIM\\([`"]ISRCTN_Ids[`"]\\)', perl = TRUE)
  testthat::expect_match(sql_isr, 'FROM\\s+[`"]ISRCTN_PM[`"]\\s+p', perl = TRUE)

  sql_nihr <- pubmed_sql(con, "nihr")
  testthat::expect_match(sql_nihr, 'TRIM\\([`"]NIHR_Ids[`"]\\)', perl = TRUE)
  testthat::expect_match(sql_nihr, 'FROM\\s+[`"]NIHR_PM[`"]\\s+p', perl = TRUE)

  sql_eu <- pubmed_sql(con, "EU")
  testthat::expect_match(sql_eu, 'TRIM\\([`"]EU_Ids[`"]\\)', perl = TRUE)
  testthat::expect_match(sql_eu, 'FROM\\s+[`"]EU_PM[`"]\\s+p', perl = TRUE)
})

testthat::test_that("pubmed_sql errors on unknown reg", {
  con <- create_sqlite_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  testthat::expect_error(pubmed_sql(con, "WHOOPS"), "Unknown reg", fixed = TRUE)
})
