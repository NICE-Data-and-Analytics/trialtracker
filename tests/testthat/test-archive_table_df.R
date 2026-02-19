testthat::test_that("archive_table_df returns friendly Message when connection invalid or table missing", {
  df1 <- archive_table_df(
    con = NULL,
    table = "NCT_rxv",
    rename_map = c("NCT ID" = "NCTId")
  )
  testthat::expect_identical(names(df1), "Message")

  con <- create_sqlite_con()
  df2 <- archive_table_df(
    con = con,
    table = "NCT_rxv",
    rename_map = c("NCT ID" = "NCTId")
  )
  testthat::expect_identical(names(df2), "Message")
  testthat::expect_match(
    df2$Message[[1]],
    "No archive table found",
    fixed = TRUE
  )
})

testthat::test_that("archive_table_df reads table, drops cols, renames, and converts Archive Date", {
  con <- create_sqlite_con()

  DBI::dbExecute(
    con,
    '
    CREATE TABLE "NCT_rxv" (
      "Query_Date" INTEGER,
      "NCTId" TEXT,
      "Junk" TEXT
    );
  '
  )
  DBI::dbWriteTable(
    con,
    "NCT_rxv",
    data.frame(
      Query_Date = 1L,
      NCTId = "NCT00000001",
      Junk = "x",
      stringsAsFactors = FALSE
    ),
    append = TRUE
  )

  df <- archive_table_df(
    con = con,
    table = "NCT_rxv",
    rename_map = c("NCT ID" = "NCTId"),
    drop_cols = c("Junk")
  )

  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_true("Archive Date" %in% names(df))
  testthat::expect_true("NCT ID" %in% names(df))
  testthat::expect_false("Junk" %in% names(df))
  testthat::expect_s3_class(df[["Archive Date"]], "Date")
  testthat::expect_identical(df[["Archive Date"]][[1]], as.Date("1970-01-02"))
})
