# helper-db.R
# Not a unit test file - just for testing various functions across DB tests

create_sqlite_con <- function() {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con), envir = parent.frame())
  con
}

create_trial_ids <- function(con) {
  DBI::dbExecute(
    con,
    '
    CREATE TABLE "Trial_Ids" (
      "NCT_Ids" TEXT,
      "EU_Ids" TEXT,
      "ISRCTN_Ids" TEXT,
      "NIHR_Ids" TEXT
    );
  '
  )
}

create_registry_tables <- function(con) {
  DBI::dbExecute(
    con,
    '
    CREATE TABLE "NCT" (
      "NCTId" TEXT,
      "Query_Date" INTEGER,
      "Program" TEXT,
      "Guideline.number" TEXT,
      "URL" TEXT,
      "OrgStudyId" TEXT,
      "Condition" TEXT,
      "BriefTitle" TEXT,
      "Acronym" TEXT,
      "OverallStatus" TEXT,
      "PrimaryCompletionDate" TEXT,
      "CompletionDate" TEXT,
      "ResultsFirstSubmitDate" TEXT,
      "ResultsFirstPostDate" TEXT,
      "LastUpdatePostDate" TEXT
    );
  '
  )

  DBI::dbExecute(
    con,
    '
    CREATE TABLE "EU" (
      "EU_Ids" TEXT,
      "Query_Date" INTEGER,
      "Program" TEXT,
      "Guideline.number" TEXT,
      "X_id" TEXT,
      "a3_full_title_of_the_trial" TEXT,
      "a32_name_or_abbreviated_title_of_the_trial_where_available" TEXT,
      "p_end_of_trial_status" TEXT,
      "a41_sponsors_protocol_code_number" TEXT,
      "URL" TEXT
    );
  '
  )

  DBI::dbExecute(
    con,
    '
    CREATE TABLE "ISRCTN" (
      "ISRCTN_No" TEXT,
      "Query_Date" INTEGER,
      "Program" TEXT,
      "Guideline.number" TEXT,
      "URL" TEXT,
      "Public_Title" TEXT,
      "Acronym" TEXT,
      "Scientific_Title" TEXT,
      "Recruitment_Status" TEXT,
      "Results_date_completed" TEXT,
      "Results_url_link" TEXT,
      "Results_summary" TEXT,
      "Results_date_posted" TEXT,
      "Results_date_first_publication" TEXT
    );
  '
  )

  DBI::dbExecute(
    con,
    '
    CREATE TABLE "NIHR" (
      "project_id" TEXT,
      "Query_Date" INTEGER,
      "Program" TEXT,
      "Guideline.number" TEXT,
      "URL" TEXT,
      "project_title" TEXT,
      "project_status" TEXT,
      "end_date" TEXT
    );
  '
  )
}

insert_trial_ids <- function(
  con,
  nct = NULL,
  eu = NULL,
  isr = NULL,
  nihr = NULL
) {
  df <- data.frame(
    NCT_Ids = nct,
    EU_Ids = eu,
    ISRCTN_Ids = isr,
    NIHR_Ids = nihr,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "Trial_Ids", df, append = TRUE)
}

insert_trial_ids <- function(
  con,
  nct = NA_character_,
  eu = NA_character_,
  isr = NA_character_,
  nihr = NA_character_
) {
  testthat::skip_if_not_installed("DBI")

  # Force 1-row values; treat NULL / character(0) as NA
  norm1 <- function(x) {
    if (is.null(x) || length(x) == 0L) {
      return(NA_character_)
    }
    as.character(x[[1]])
  }

  row <- data.frame(
    NCT_Ids = norm1(nct),
    EU_Ids = norm1(eu),
    ISRCTN_Ids = norm1(isr),
    NIHR_Ids = norm1(nihr),
    stringsAsFactors = FALSE
  )

  DBI::dbAppendTable(con, "Trial_Ids", row)
  invisible(row)
}
