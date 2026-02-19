test_that("with_scaffold returns SQL and contains expected CTEs", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  res <- with_scaffold(
    source_table    = "Source_Table",
    id_col          = "Trial_ID",
    ids_source_col  = "NCT",
    lookback_days   = 31L,
    ids_table       = "Trial_Ids",
    con             = con
  )

  expect_true(methods::is(res, "SQL"))

  sql <- as.character(res)

  expect_match(sql, "INNER JOIN\\s+ids\\s+USING\\(\\s*`Trial_ID`\\s*\\)", perl = TRUE)
  expect_match(sql, "FROM\\s+`Trial_Ids`", perl = TRUE)
  expect_match(sql, "FROM\\s+`Source_Table`", perl = TRUE)
  expect_match(sql, "l\\.latest_qd\\s*-\\s*31\\b", perl = TRUE)
})
