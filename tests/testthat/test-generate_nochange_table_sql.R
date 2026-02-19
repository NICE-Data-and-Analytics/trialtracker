testthat::test_that("generate_nochange_table_sql returns stable df and parses Date Added", {
  con <- create_sqlite_con()
  create_trial_ids(con)
  create_registry_tables(con)

  insert_trial_ids(con, nct = "  NCT00000001 ")

  # Query_Date must be numeric day-count since 1970-01-01
  qd_old <- as.integer(as.Date("2018-01-01") - as.Date("1970-01-01"))
  qd_new <- as.integer(as.Date("2024-01-01") - as.Date("1970-01-01"))

  # Two snapshots with identical join keys (Query_Date differs; ignored in join)
  DBI::dbWriteTable(
    con,
    "NCT",
    data.frame(
      NCTId = c("NCT00000001", "NCT00000001"),
      Query_Date = c(qd_old, qd_new),
      Program = c("P1", "P1"),
      `Guideline.number` = c("G1", "G1"),
      URL = c("u_same", "u_same"), # IMPORTANT: must match to count as "no change"
      OrgStudyId = c("O1", "O1"),
      Condition = c("C1", "C1"),
      BriefTitle = c("T1", "T1"),
      Acronym = c("A1", "A1"),
      OverallStatus = c("Recruiting", "Recruiting"),
      PrimaryCompletionDate = c("2020-01-01", "2020-01-01"),
      CompletionDate = c("2021-01-01", "2021-01-01"),
      ResultsFirstSubmitDate = c(NA, NA),
      ResultsFirstPostDate = c(NA, NA),
      LastUpdatePostDate = c("2023-01-01", "2023-01-01"),
      stringsAsFactors = FALSE
    ),
    append = TRUE
  )

  res <- generate_nochange_table_sql(
    conn = con,
    table_name = "NCT",
    id_col = "NCTId",
    rename_list = c("Guideline" = "Guideline.number", "NCT ID" = "NCTId"),
    reorder_cols = c("Program", "Guideline", "Date Added", "NCT ID", "URL"),
    months_back = 60L,
    qdate_col = "Query_Date",
    comments_col = "Comments" # absent; should be handled via intersect()
  )

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_true(nrow(res) >= 1)

  # Column order must match reorder_cols
  testthat::expect_identical(
    names(res),
    c("Program", "Guideline", "Date Added", "NCT ID", "URL")
  )

  # Date Added parsed to Date
  testthat::expect_s3_class(res[["Date Added"]], "Date")

  # Tracked trial included
  testthat::expect_true(any(res[["NCT ID"]] == "NCT00000001"))
})

testthat::test_that("generate_nochange_table_sql errors when no old snapshot exists", {
  con <- create_sqlite_con()
  create_trial_ids(con)
  create_registry_tables(con)

  insert_trial_ids(con, nct = "NCT00000001")

  # Query_Date must be numeric day-count since 1970-01-01
  qd_new <- as.integer(as.Date("2024-01-01") - as.Date("1970-01-01"))

  # Insert ONLY the current snapshot. With months_back = 60, target_qd will be
  # about 5 years earlier than current, so there will be no snapshot <= target_qd.
  DBI::dbWriteTable(
    con,
    "NCT",
    data.frame(
      NCTId = "NCT00000001",
      Query_Date = qd_new,
      Program = "P1",
      `Guideline.number` = "G1",
      URL = "u_new",
      OrgStudyId = "O1",
      Condition = "C1",
      BriefTitle = "T1",
      Acronym = "A1",
      OverallStatus = "Recruiting",
      PrimaryCompletionDate = NA,
      CompletionDate = NA,
      ResultsFirstSubmitDate = NA,
      ResultsFirstPostDate = NA,
      LastUpdatePostDate = NA,
      stringsAsFactors = FALSE
    ),
    append = TRUE
  )

  testthat::expect_error(
    generate_nochange_table_sql(
      conn = con,
      table_name = "NCT",
      id_col = "NCTId",
      months_back = 60L
    ),
    "No snapshot found on/before",
    fixed = TRUE
  )
})


testthat::test_that("generate_nochange_table_sql validates reorder_cols against drops", {
  con <- create_sqlite_con()
  create_trial_ids(con)
  create_registry_tables(con)

  insert_trial_ids(con, nct = "NCT00000001")

  qd_old <- as.integer(as.Date("2018-01-01") - as.Date("1970-01-01"))
  qd_new <- as.integer(as.Date("2024-01-01") - as.Date("1970-01-01"))

  DBI::dbWriteTable(
    con,
    "NCT",
    data.frame(
      NCTId = c("NCT00000001", "NCT00000001"),
      Query_Date = c(qd_old, qd_new),
      Program = "P1",
      `Guideline.number` = "G1",
      URL = "u1",
      OrgStudyId = "O1",
      Condition = "C1",
      BriefTitle = "T1",
      Acronym = "A1",
      OverallStatus = "Recruiting",
      PrimaryCompletionDate = NA,
      CompletionDate = NA,
      ResultsFirstSubmitDate = NA,
      ResultsFirstPostDate = NA,
      LastUpdatePostDate = NA,
      stringsAsFactors = FALSE
    ),
    append = TRUE
  )

  testthat::expect_error(
    generate_nochange_table_sql(
      conn = con,
      table_name = "NCT",
      id_col = "NCTId",
      drop_cols = c("URL"),
      reorder_cols = c("Program", "URL", "Date Added"),
      months_back = 60L
    ),
    "reorder_cols contains a column not available",
    fixed = TRUE
  )
})

testthat::test_that("generate_nochange_table_sql errors clearly for unsupported table_name", {
  con <- create_sqlite_con()

  testthat::expect_error(
    generate_nochange_table_sql(
      conn = con,
      table_name = "WHOOPS",
      id_col = "id"
    ),
    "unsupported table_name",
    fixed = FALSE
  )
})
