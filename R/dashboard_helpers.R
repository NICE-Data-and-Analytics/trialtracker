#' Dashboard helper functions (SQL generation, DB access, and text sanitisation)
#'
#' Internal utilities used by the trialtracker dashboard. This file contains:
#' \itemize{
#'   \item SQL code-generation helpers that emit common CTE and other scaffolding
#'   \item Safe wrappers around DBI queries to prevent UI-breaking errors
#'   \item Text cleaning and encoding utilities for reliable rendering
#' }
#'
#' These functions are not part of the public package API. They exist to:
#' \itemize{
#'   \item Keep the dashboard Rmd thin and focused on UI/server wiring
#'   \item Centralise SQL-generation logic in testable package code
#'   \item Provide defensive handling of database and encoding edge cases
#' }
#'
#' @keywords internal
NULL

# ------------------------------------------------------------------------------
# SQL scaffolding
# ------------------------------------------------------------------------------

#' Emit common CTE scaffolding used by dashboard SQL queries
#'
#' Generates a DBI SQL fragment containing the shared CTE structure used across
#' snapshot-comparison queries in the dashboard. The emitted CTEs are:
#' \itemize{
#'   \item \code{ids}: distinct, trimmed IDs from \code{ids_table}
#'   \item \code{first_seen}: earliest observed \code{Query_Date} per ID (as a calendar date)
#'   \item \code{latest}: most recent \code{Query_Date} per ID
#'   \item \code{n}: rows from \code{source_table} at the latest snapshot per ID
#'   \item \code{o_pick}: most recent row on or before \code{latest_qd - lookback_days}
#'   \item \code{o}: rows from \code{source_table} at the selected older snapshot per ID
#' }
#'
#' Assumes \code{source_table} contains a numeric \code{Query_Date} column expressed
#' as days since 1970-01-01. This function does not execute SQL; it only generates
#' a safely quoted fragment for inclusion in larger queries.
#'
#' @param source_table Table/view containing historical snapshots.
#' @param id_col Identifier column used for joins.
#' @param ids_source_col Column in \code{ids_table} containing raw ID values.
#' @param lookback_days Number of days to look back when selecting the "old" snapshot.
#' @param ids_table Table containing the list of IDs to include.
#' @param con DBI connection used for identifier quoting in \code{glue::glue_sql()}.
#' @return A DBI SQL object.
#' @keywords internal
with_scaffold <- function(
  source_table,
  id_col,
  ids_source_col,
  lookback_days = 31L,
  ids_table = "Trial_Ids",
  con = pool_con
) {
  glue::glue_sql(
    '
ids AS (
  SELECT DISTINCT TRIM({`ids_source_col`}) AS {`id_col`}
  FROM {`ids_table`}
  WHERE {`ids_source_col`} IS NOT NULL AND TRIM({`ids_source_col`}) <> \'\'
),
first_seen AS (
  SELECT {`id_col`}, DATE(\'1970-01-01\', \'+\' || MIN(Query_Date) || \' days\') AS "Date Added"
  FROM {`source_table`}
  INNER JOIN ids USING({`id_col`})
  GROUP BY {`id_col`}
),
latest AS (
  SELECT {`id_col`},
         MAX(Query_Date) AS latest_qd
  FROM {`source_table`}
  INNER JOIN ids USING({`id_col`})
  GROUP BY {`id_col`}
),
n AS (
  SELECT t.*
  FROM {`source_table`} t
  INNER JOIN latest l USING({`id_col`})
  WHERE t.Query_Date = l.latest_qd
),
o_pick AS (
  SELECT l.{`id_col`},
         MAX(t.Query_Date) AS old_qd
  FROM latest l
  INNER JOIN {`source_table`} t USING({`id_col`})
  WHERE t.Query_Date <= (l.latest_qd - {lookback_days})
  GROUP BY l.{`id_col`}
),
o AS (
  SELECT t.*
  FROM {`source_table`} t
  INNER JOIN o_pick p USING({`id_col`})
  WHERE t.Query_Date = p.old_qd
)
',
    .con = con
  )
}

# --- NCT “Latest” SQL builder (no temp tables; exact columns/order) ---
nct_latest_sql_from_trial_ids <- function(con) {
  glue::glue_sql(
    "
WITH
  ids AS (
    SELECT DISTINCT TRIM(\"NCT_Ids\") AS \"NCTId\"
    FROM \"Trial_Ids\"
    WHERE \"NCT_Ids\" IS NOT NULL AND TRIM(\"NCT_Ids\") <> ''
  ),
  base AS (
    SELECT n.*
    FROM \"NCT\" n
    JOIN ids USING (\"NCTId\")
  ),
  latest_rank AS (
    SELECT base.*,
           ROW_NUMBER() OVER (PARTITION BY \"NCTId\" ORDER BY \"Query_Date\" DESC) AS rn
    FROM base
  ),
  latest AS (SELECT * FROM latest_rank WHERE rn = 1),
  first_seen AS (
    SELECT \"NCTId\", DATE('1970-01-01', '+' || MIN(\"Query_Date\") || ' days') AS \"Date Added\"
    FROM base
    GROUP BY \"NCTId\"
  )
SELECT
  latest.\"Program\"                 AS \"Program\",
  latest.\"Guideline.number\"        AS \"Guideline\",
  first_seen.\"Date Added\"          AS \"Date Added\",
  latest.\"URL\"                     AS \"URL\",
  latest.\"NCTId\"                   AS \"NCT ID\",
  latest.\"OrgStudyId\"              AS \"Org Study ID\",
  latest.\"Condition\"               AS \"Condition\",
  latest.\"BriefTitle\"              AS \"Title\",
  latest.\"Acronym\"                 AS \"Acronym\",
  latest.\"OverallStatus\"           AS \"Status\",
  latest.\"PrimaryCompletionDate\"   AS \"Primary Completion Date\",
  latest.\"CompletionDate\"          AS \"Completion Date\",
  latest.\"ResultsFirstSubmitDate\"  AS \"Results Submit Date\",
  latest.\"ResultsFirstPostDate\"    AS \"Results Post Date\",
  latest.\"LastUpdatePostDate\"      AS \"Last Update\"
FROM latest
JOIN first_seen USING (\"NCTId\")
ORDER BY \"Date Added\" DESC, \"NCT ID\";
",
    .con = con
  )
}

# --- EU “Latest” SQL builder (filter via Trial_Ids.EU_Ids; exact columns/order; no temp tables) ---
eu_latest_sql_from_trial_ids <- function(con) {
  glue::glue_sql(
    "
WITH
  ids AS (
    SELECT DISTINCT TRIM(\"EU_Ids\") AS \"EU_Ids\"
    FROM \"Trial_Ids\"
    WHERE \"EU_Ids\" IS NOT NULL AND TRIM(\"EU_Ids\") <> ''
  ),
  base AS (
    SELECT e.*
    FROM \"EU\" e
    JOIN ids USING (\"EU_Ids\")
  ),
  latest_rank AS (
    SELECT base.*,
           ROW_NUMBER() OVER (PARTITION BY \"EU_Ids\" ORDER BY \"Query_Date\" DESC) AS rn
    FROM base
  ),
  latest AS (SELECT * FROM latest_rank WHERE rn = 1),
  first_seen AS (
    SELECT \"EU_Ids\", DATE('1970-01-01', '+' || MIN(\"Query_Date\") || ' days') AS \"Date Added\"
    FROM base
    GROUP BY \"EU_Ids\"
  )
SELECT
  latest.\"Program\"                                                   AS \"Program\",
  latest.\"Guideline.number\"                                          AS \"Guideline\",
  first_seen.\"Date Added\"                                            AS \"Date Added\",
  latest.\"X_id\"                                                      AS \"Clinicaltrials.eu ID\",
  latest.\"a3_full_title_of_the_trial\"                                AS \"Title\",
  latest.\"a32_name_or_abbreviated_title_of_the_trial_where_available\" AS \"Abbreviated Title\",
  latest.\"p_end_of_trial_status\"                                     AS \"End of Trial Status\",
  latest.\"a41_sponsors_protocol_code_number\"                         AS \"Sponsor ID\",
  latest.\"URL\"                                                       AS \"URL\"
FROM latest
JOIN first_seen USING (\"EU_Ids\")
ORDER BY \"Date Added\" DESC, \"Clinicaltrials.eu ID\";
",
    .con = con
  )
}

## --- ISRCTN “Latest” SQL builder (non-reactive) ---
isr_latest_sql_from_trial_ids <- function(con) {
  glue::glue_sql(
    "
WITH
  ids AS (
    SELECT DISTINCT TRIM(\"ISRCTN_Ids\") AS \"ISRCTN_No\"
    FROM \"Trial_Ids\"
    WHERE \"ISRCTN_Ids\" IS NOT NULL AND TRIM(\"ISRCTN_Ids\") <> ''
  ),
  base AS (
    SELECT i.*
    FROM \"ISRCTN\" i
    JOIN ids USING (\"ISRCTN_No\")
  ),
  latest_rank AS (
    SELECT base.*,
           ROW_NUMBER() OVER (PARTITION BY \"ISRCTN_No\" ORDER BY \"Query_Date\" DESC) AS rn
    FROM base
  ),
  latest AS (SELECT * FROM latest_rank WHERE rn = 1),
  first_seen AS (
    SELECT \"ISRCTN_No\", DATE('1970-01-01', '+' || MIN(\"Query_Date\") || ' days') AS \"Date Added\"
    FROM base
    GROUP BY \"ISRCTN_No\"
  )
SELECT
  latest.\"Program\"                      AS \"Program\",
  latest.\"Guideline.number\"             AS \"Guideline\",
  first_seen.\"Date Added\"               AS \"Date Added\",
  latest.\"URL\"                          AS \"URL\",
  latest.\"ISRCTN_No\"                    AS \"ISRCTN ID\",
  latest.\"Public_Title\"                 AS \"Title\",
  latest.\"Acronym\"                      AS \"Acronym\",
  latest.\"Scientific_Title\"             AS \"Scientific Title\",
  latest.\"Recruitment_Status\"           AS \"Recruitment Status\",
  latest.\"Results_date_completed\"       AS \"Results Completed Date\",
  latest.\"Results_url_link\"             AS \"Results URL Link\",
  latest.\"Results_summary\"              AS \"Results Summary\",
  latest.\"Results_date_posted\"          AS \"Results Posted Date\",
  latest.\"Results_date_first_publication\" AS \"Results Published Date\"
FROM latest
JOIN first_seen USING (\"ISRCTN_No\")
ORDER BY \"Date Added\" DESC, \"ISRCTN ID\";
",
    .con = con
  )
}

# --- NIHR “Latest” SQL builder (filters via Trial_Ids; normalizes IDs) ---
nihr_latest_sql_from_trial_ids <- function(con) {
  glue::glue_sql(
    "
WITH
  ids AS (
    SELECT DISTINCT
      TRIM(\"NIHR_Ids\")                              AS \"NIHR_Ids_raw\",
      REPLACE(REPLACE(TRIM(\"NIHR_Ids\"),'-',''),'/','') AS \"id_norm\"
    FROM \"Trial_Ids\"
    WHERE \"NIHR_Ids\" IS NOT NULL AND TRIM(\"NIHR_Ids\") <> ''
  ),
  base AS (
    SELECT n.*
    FROM \"NIHR\" n
    JOIN ids
      ON REPLACE(REPLACE(n.\"project_id\",'-',''),'/','') = ids.\"id_norm\"
  ),
  latest_rank AS (
    SELECT base.*,
           ROW_NUMBER() OVER (PARTITION BY \"project_id\" ORDER BY \"Query_Date\" DESC) AS rn
    FROM base
  ),
  latest AS (SELECT * FROM latest_rank WHERE rn = 1),
  first_seen AS (
    SELECT \"project_id\", DATE('1970-01-01', '+' || MIN(\"Query_Date\") || ' days') AS \"Date Added\"
    FROM base
    GROUP BY \"project_id\"
  )
SELECT
  latest.\"Program\"         AS \"Program\",
  latest.\"Guideline.number\" AS \"Guideline\",
  first_seen.\"Date Added\"  AS \"Date Added\",
  latest.\"URL\"             AS \"URL\",
  latest.\"project_id\"      AS \"NIHR ID\",
  latest.\"project_title\"   AS \"Project Title\",
  latest.\"project_status\"  AS \"Status\",
  latest.\"end_date\"        AS \"End Date\"
FROM latest
JOIN first_seen USING (\"project_id\")
ORDER BY \"Date Added\" DESC, \"NIHR ID\";
",
    .con = con
  )
}

# --- Build SQL for a registry's PubMed table (Trial_Ids filtering) ---
pubmed_sql <- function(con, reg) {
  pm_tbl <- paste0(toupper(reg), "_PM")

  # Map registry to the Trial_Ids column used for tracking
  ids_col <- switch(
    toupper(reg),
    "NCT" = "NCT_Ids",
    "ISRCTN" = "ISRCTN_Ids",
    "NIHR" = "NIHR_Ids",
    "EU" = "EU_Ids",
    stop("Unknown reg in pubmed_sql(): ", reg)
  )

  glue::glue_sql(
    '
WITH ids AS (
  SELECT DISTINCT TRIM({`ids_col`}) AS "Trial_ID"
  FROM "Trial_Ids"
  WHERE {`ids_col`} IS NOT NULL
    AND TRIM({`ids_col`}) <> \'\'
)
SELECT
  p."Program"                AS "Program",
  p."Guideline.number"       AS "Guideline",
  p."Query_Date"             AS "Query_Date",
  p."ID"                     AS "Trial ID",
  p."pmid"                   AS "pmid",
  p."doi"                    AS "Publication DOI",
  p."Title"                  AS "Title",
  p."Abstract"               AS "Abstract",
  p."Journal"                AS "Journal",
  p."Short..working.title."   AS "Working Title"
FROM {`pm_tbl`} p
INNER JOIN ids
  ON TRIM(p."ID") = ids."Trial_ID"
ORDER BY p."Query_Date" DESC, p."pmid";
',
    pm_tbl = pm_tbl,
    ids_col = ids_col,
    .con = con
  ) |>
    as.character()
}

generate_nochange_table_sql <- function(
  conn,
  table_name,
  id_col,
  rename_list = NULL, # named: new_name = old_name
  drop_cols = NULL, # old/original column names
  reorder_cols = NULL, # final/output names (post-rename), may include "Date Added"
  date_cols = NULL, # old/original query date cols to parse in R (numeric to ISO format)
  extra_mutate = NULL, # 'extra mutate' to format other date columns
  months_back = 6L, # default lookback period in months
  qdate_col = "Query_Date",
  comments_col = "Comments",
  date_added_name = "Date Added"
) {
  cols <- DBI::dbListFields(conn, table_name)

  as_int1 <- function(x, what = "value") {
    n <- suppressWarnings(as.numeric(x))
    if (length(n) != 1 || is.na(n)) {
      stop(what, " is not numeric/integer-like: ", paste(x, collapse = ", "))
    }
    as.integer(n)
  }

  # ---- Snapshot dates (Query_Date is numeric day-count; CAST for safety) ----
  current_qd_raw <- DBI::dbGetQuery(
    conn,
    glue::glue_sql(
      "SELECT MAX({`qdate`}) AS qd FROM {`tbl`}",
      qdate = qdate_col,
      tbl = table_name,
      .con = conn
    )
  )$qd[[1]]

  current_qd <- as_int1(current_qd_raw, "current_qd")
  current_date <- as.Date(current_qd, origin = "1970-01-01")

  target_date <- lubridate::`%m-%`(
    current_date,
    lubridate::period(months = as.integer(months_back))
  )
  target_qd <- as.integer(target_date)

  old_qd_raw <- DBI::dbGetQuery(
    conn,
    glue::glue_sql(
      "SELECT MAX({`qdate`}) AS qd
       FROM {`tbl`}
       WHERE {`qdate`} <= {target_qd}",
      qdate = qdate_col,
      tbl = table_name,
      target_qd = target_qd,
      .con = conn
    )
  )$qd[[1]]

  old_qd <- as_int1(old_qd_raw, "old_qd")
  if (is.na(old_qd)) {
    stop(
      "No snapshot found on/before ",
      as.character(target_date),
      " in table ",
      table_name
    )
  }

  # ---- Rename maps (rename_list: new = old) ----
  new_to_old <- if (!is.null(rename_list)) rename_list else character(0)
  old_to_new <- if (!is.null(rename_list)) {
    setNames(names(rename_list), unname(rename_list))
  } else {
    character(0)
  }

  # ---- Define "no change" join keys: everything except Query_Date and Comments ----
  ignore_cols <- intersect(c(qdate_col, comments_col), cols)
  join_keys <- setdiff(cols, ignore_cols)
  if (!length(join_keys)) {
    stop(
      "No join keys left after excluding ",
      paste(ignore_cols, collapse = ", ")
    )
  }

  base_out_old <- join_keys
  if (!is.null(drop_cols)) {
    base_out_old <- setdiff(base_out_old, drop_cols)
  }

  # ---- SELECT list (rename + reorder pushed into SQL) ----
  select_items <- character()

  add_col <- function(old, alias) {
    select_items <<- c(
      select_items,
      as.character(glue::glue_sql(
        "c.{`old`} AS {`alias`}",
        old = old,
        alias = alias,
        .con = conn
      ))
    )
  }
  add_date_added <- function() {
    select_items <<- c(
      select_items,
      as.character(glue::glue_sql(
        "date_added.{`da`} AS {`da`}",
        da = date_added_name,
        .con = conn
      ))
    )
  }

  if (!is.null(reorder_cols)) {
    for (nm in reorder_cols) {
      if (identical(nm, date_added_name)) {
        add_date_added()
        next
      }
      old <- if (nm %in% names(new_to_old)) new_to_old[[nm]] else nm
      if (!old %in% base_out_old) {
        stop(
          "reorder_cols contains a column not available after drops/ignores: ",
          nm,
          " (maps to old='",
          old,
          "')"
        )
      }
      add_col(old, nm)
    }
  } else {
    for (old in base_out_old) {
      alias <- if (old %in% names(old_to_new)) old_to_new[[old]] else old
      add_col(old, alias)
    }
    add_date_added()
  }

  select_list_sql <- paste(select_items, collapse = ", ")

  # Join condition: use IS so NULL matches NULL (dplyr-like)
  on_sql <- glue::glue_sql("c.{`k`} IS o.{`k`}", k = join_keys, .con = conn) |>
    glue::glue_sql_collapse(sep = " AND ")

  # Only pull join_keys in current/old — enough to compare + output
  join_keys_sql <- glue::glue_sql("{`k`*}", k = join_keys, .con = conn)

  # Date Added (numeric day-count): MIN(Query_Date) - 1
  date_added_expr <- glue::glue_sql(
    "MIN({`qdate`}) - 1",
    qdate = qdate_col,
    .con = conn
  )

  # Generate "Trial_Ids" sql table column
  trial_ids_col <- switch(
    toupper(table_name),
    "NCT" = "NCT_Ids",
    "ISRCTN" = "ISRCTN_Ids",
    "NIHR" = "NIHR_Ids",
    "EU" = "EU_Ids",
    NULL
  )

  sql <- glue::glue_sql(
    "
    WITH
    ids AS (
    SELECT DISTINCT TRIM({`trial_ids_col`}) AS \"Trial_ID\"
    FROM \"Trial_Ids\"
    WHERE {`trial_ids_col`} IS NOT NULL
      AND TRIM({`trial_ids_col`}) <> ''
    ),
    current AS (
      SELECT {DBI::SQL(join_keys_sql)}
      FROM {`tbl`} t
      INNER JOIN ids i
        ON TRIM(t.{`id`}) = i.\"Trial_ID\"
      WHERE {`qdate`} = {current_qd}
    ),
    old AS (
      SELECT {DBI::SQL(join_keys_sql)}
      FROM {`tbl`} t
      INNER JOIN ids i
        ON TRIM(t.{`id`}) = i.\"Trial_ID\"
      WHERE {`qdate`} = {old_qd}
    ),
    date_added AS (
      SELECT {`id`} AS {`id`},
             {DBI::SQL(date_added_expr)} AS {`da`}
      FROM {`tbl`}
      GROUP BY {`id`}
    )
    SELECT DISTINCT
      {DBI::SQL(select_list_sql)}
    FROM current AS c
    INNER JOIN old AS o
      ON {DBI::SQL(on_sql)}
    LEFT JOIN date_added
      ON c.{`id`} = date_added.{`id`}
    ",
    tbl = table_name,
    qdate = qdate_col,
    id = id_col,
    da = date_added_name,
    current_qd = current_qd,
    old_qd = old_qd,
    .con = conn
  )

  res <- DBI::dbGetQuery(conn, sql)

  # ---- R-side post-processing ----
  if (exists("trialtracker:::clean_utf8", mode = "function")) {
    res[] <- lapply(res, trialtracker:::clean_utf8)
  }

  if (date_added_name %in% names(res)) {
    res[[date_added_name]] <- as.Date(
      as.integer(res[[date_added_name]]),
      origin = "1970-01-01"
    )
  }

  # date_cols are old/original names; map to output names if renamed in SQL, then parse
  if (!is.null(date_cols)) {
    for (old_dc in date_cols) {
      out_dc <- if (old_dc %in% names(old_to_new)) {
        old_to_new[[old_dc]]
      } else {
        old_dc
      }
      if (out_dc %in% names(res)) {
        res[[out_dc]] <- lubridate::as_date(
          lubridate::parse_date_time(
            res[[out_dc]],
            c("ymd", "bdy", "dmy"),
            truncated = 1
          )
        )
      }
    }
  }

  if (is.function(extra_mutate)) {
    res <- extra_mutate(res)
  }

  unique(res)
}

# ------------------------------------------------------------------------------
# Database helpers
# ------------------------------------------------------------------------------

#' Safe wrapper around DBI::dbGetQuery for dashboard use
#'
#' Executes a query and guarantees a data.frame return value. On failure (missing
#' connection or query error), logs the issue and returns a single-column
#' data.frame containing the error message. This keeps DT outputs stable and
#' prevents UI crashes during rendering.
#'
#' @param con A DBI connection.
#' @param sql SQL string or DBI::SQL object.
#' @param label Short label identifying the caller (used in error messages/logs).
#' @return A data.frame containing query results, or a data.frame with an \code{Error} column.
#' @keywords internal
safe_db_getquery <- function(con, sql, label) {
  if (is.null(con)) {
    msg <- paste0(label, ": no DB connection")
    log_msg("DB ERROR:", msg)
    return(data.frame(Error = msg, stringsAsFactors = FALSE))
  }

  tryCatch(
    {
      df <- DBI::dbGetQuery(con, sql)
      if (is.null(df)) {
        df <- data.frame()
      }
      df
    },
    error = function(e) {
      msg <- paste0(label, ": ", conditionMessage(e))
      log_msg("DB ERROR:", msg)
      data.frame(Error = msg, stringsAsFactors = FALSE)
    }
  )
}

# ------------------------------------------------------------------------------
# Encoding and text sanitisation
# ------------------------------------------------------------------------------

#' Coerce input to UTF-8 character encoding
#'
#' Lightweight helper to coerce input to character and ensure UTF-8 encoding,
#' used before storing text in the database or rendering it in UI widgets.
#'
#' @param x Vector to coerce.
#' @return UTF-8 encoded character vector.
#' @keywords internal
clean_utf8 <- function(x) enc2utf8(as.character(x))

#' Truncate strings for UI display
#'
#' Fast truncation helper intended for table rendering. Strings longer than
#' \code{n} characters are truncated and suffixed with an ellipsis.
#'
#' @param x Character vector (coerced via \code{as.character}).
#' @param n Maximum number of characters to retain.
#' @return Truncated character vector.
#' @keywords internal
fast_trunc <- function(x, n = 120L) {
  # UI-only truncation, cheap
  x <- as.character(x)
  i <- nchar(x) > n
  x[i] <- paste0(substr(x[i], 1L, n), "…")
  x
}

#' Clean PubMed metadata text for safe rendering
#'
#' PubMed metadata sometimes includes Windows-1252 bytes, invalid UTF-8 sequences,
#' and control/format characters that can break widgets or DT rendering. This
#' helper performs best-effort conversion to valid UTF-8 and strips problematic
#' control characters.
#'
#' @param x Character or factor vector.
#' @return Cleaned UTF-8 character vector.
#' @keywords internal
clean_text_pubmed <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (!is.character(x)) {
    return(x)
  }

  # Convert Windows-1252 bytes (e.g. \x92) to proper UTF-8
  x2 <- iconv(x, from = "CP1252", to = "UTF-8", sub = "")

  # If anything is *still* not valid UTF-8, force it through ICU (best-effort)
  ok <- suppressWarnings(stringi::stri_enc_isutf8(x2))
  if (any(!ok, na.rm = TRUE)) {
    x2[!ok] <- stringi::stri_enc_toutf8(x2[!ok])
  }

  # Drop control/format chars that sometimes also break widgets
  x2 <- stringi::stri_replace_all_regex(x2, "[\\p{Cf}\\p{Cc}]", "")
  x2
}

#' Coerce PubMed-style date values to Date
#'
#' Converts a vector of PubMed date values stored as "days since 1970-01-01"
#' into an R \code{Date}. Non-numeric values (including empty strings) are
#' converted to \code{NA}.
#'
#' This helper is intentionally permissive and is used when preparing PubMed
#' fields for display and downstream comparisons.
#'
#' @param x Vector containing numeric-like values representing days since 1970-01-01.
#' @return A \code{Date} vector with \code{NA} where conversion is not possible.
#' @keywords internal
coerce_pm_date <- function(x) {
  num <- suppressWarnings(as.numeric(x))
  d <- ifelse(is.finite(num), num, NA_real_)
  as.Date(d, origin = "1970-01-01")
}

#' Truncate strings to a fixed width
#'
#' Simple string truncation helper used for UI display (e.g., abstracts or long
#' text fields). Values longer than \code{n} characters are truncated and
#' suffixed with an ellipsis.
#'
#' @param x Character vector (will be coerced with \code{as.character}).
#' @param n Maximum number of characters to retain.
#' @return A character vector where long values are truncated.
#' @keywords internal
truncate_str <- function(x, n = 200L) {
  x <- as.character(x)
  i <- nchar(x) > n
  x[i] <- paste0(substr(x[i], 1L, n), "…")
  x
}

#' Configure TrialTracker dashboard logging
#'
#' Sets the option \code{trialtracker.log_file} (defaulting to a file in
#' \code{tempdir()}) and returns a logger function that writes timestamped lines
#' to that file and also emits them via \code{message()}.
#'
#' This mirrors the logging pattern currently defined in \code{index.Rmd}, but in
#' a reusable (and testable) package function.
#'
#' @param log_file Path to the log file. If \code{NULL}, uses
#'   \code{file.path(tempdir(), "trialtracker_shiny.log")}.
#' @return A function with signature \code{function(...)} that logs messages.
#' @keywords internal
configure_dashboard_logging <- function(log_file = NULL) {
  # 1) Choose a default log file if one is not provided ---
  if (is.null(log_file)) {
    log_file <- file.path(tempdir(), "trialtracker_shiny.log")
  }

  # 2) Store the log path in an option ---
  # normalizePath(..., mustWork = FALSE) avoids errors if the file doesn't exist yet.
  # winslash = "\\" keeps Windows-style path
  options(
    trialtracker.log_file = normalizePath(
      log_file,
      winslash = "\\",
      mustWork = FALSE
    )
  )

  # 3) Return a logger function
  # The retuned function:
  #   - reads the current option each time (so changing the option later works)
  #   - timestamps each line
  #   - appends to the log file
  #   - also calls message() so logs show up in console / Connect logs
  function(...) {
    lf <- getOption(
      "trialtracker.log_file",
      file.path(tempdir(), "trialtracker_shiny.log")
    )

    txt <- paste0(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " | ",
      paste(..., collapse = " ")
    )

    cat(txt, "\n", file = lf, append = TRUE)
    message(txt)

    invisible(txt)
  }
}
#' Resolve the TrialTracker SQLite DB path inside the app folder
#'
#' Looks for the SQLite DB at:
#'   inst/extdata/RSQLite_data/TrialTracker-db.sqlite
#' relative to the current working directory (typically the app root under
#' Shiny Server).
#'
#' @param rel_dir Relative directory containing the DB file.
#' @param db_file DB filename.
#' @return Absolute normalized path, or NA_character_ if not found.
#' @keywords internal
resolve_db_path_in_app <- function(
  rel_dir = file.path("inst", "extdata", "RSQLite_data"),
  db_file = "TrialTracker-db.sqlite"
) {
  p_rel <- file.path(rel_dir, db_file)
  tryCatch(
    normalizePath(p_rel, mustWork = TRUE),
    error = function(e) NA_character_
  )
}
#' Initialise a SQLite pool and apply dashboard PRAGMAs
#'
#' Creates a \code{pool::dbPool()} for SQLite and applies the same PRAGMAs that
#' were previously executed in \code{index.Rmd}. Returns \code{NULL} on failure
#' so the dashboard can continue to render a friendly error state.
#'
#' @param db_path_abs Absolute DB path (e.g. from \code{resolve_db_path()}).
#' @param log_fun Logger function with signature \code{function(...)}.
#' @return A pool object, or \code{NULL}.
#' @keywords internal
init_pool_with_pragmas <- function(db_path_abs, log_fun) {
  pool_con <- NULL

  if (is.na(db_path_abs) || !nzchar(db_path_abs)) {
    log_fun("DB path not found:", db_path_abs)
    return(NULL)
  }

  pool_con <- tryCatch(
    pool::dbPool(RSQLite::SQLite(), dbname = db_path_abs),
    error = function(e) {
      log_fun("DB connection failed:", conditionMessage(e))
      NULL
    }
  )
  if (is.null(pool_con)) {
    return(NULL)
  }

  pragmas <- c(
    "PRAGMA journal_mode=WAL;",
    "PRAGMA synchronous=NORMAL;",
    "PRAGMA temp_store=MEMORY;",
    "PRAGMA mmap_size=134217728;",
    "PRAGMA busy_timeout=15000;"
  )

  # Apply pragmas to a real connection from the pool.
  con <- tryCatch(pool::poolCheckout(pool_con), error = function(e) NULL)
  if (!is.null(con)) {
    on.exit(pool::poolReturn(con), add = TRUE)
    for (p in pragmas) {
      tryCatch(
        DBI::dbExecute(con, p),
        error = function(e) {
          log_fun("PRAGMA failed:", p, "->", conditionMessage(e))
        }
      )
    }
  } else {
    # Fallback: try via pool object directly (mirrors current behaviour)
    for (p in pragmas) {
      tryCatch(
        DBI::dbExecute(pool_con, p),
        error = function(e) {
          log_fun("PRAGMA failed:", p, "->", conditionMessage(e))
        }
      )
    }
  }

  log_fun("DB connected:", db_path_abs)
  pool_con
}
#' Format “Recent Status Changes” tables for dashboard display
#'
#' Pure formatting helpers used by the dashboard “Recent Status Changes” tab.
#' These functions take the raw result of a SQL query (data.frame) and return
#' a cleaned, consistently ordered data.frame ready for DT rendering.
#'
#' Keeping formatting separate from Shiny renderers makes unit testing easy and
#' keeps index.Rmd short and readable.
#'
#' @keywords internal
NULL

#' Format recent changes: ClinicalTrials.gov (NCT)
#' @param df Data frame returned by the NCT recent-changes SQL.
#' @return Formatted data frame (renamed, truncated, dates parsed, columns ordered).
#' @keywords internal
format_recent_changes_nct <- function(df) {
  keep_cols <- c(
    "Change",
    "Changed Field",
    "Program",
    "Guideline",
    "Date Added",
    "URL",
    "NCT ID",
    "Acronym",
    "Title",
    "Condition",
    "Status",
    "Completion Date",
    "Results Submit Date",
    "Last Update Date"
  )

  df |>
    dplyr::rename(Guideline = Guideline.number, `NCT ID` = NCTId) |>
    dplyr::mutate(
      URL = paste0("https://clinicaltrials.gov/ct2/show/", .data[["NCT ID"]]),
      Title = trialtracker:::fast_trunc(.data[["Title"]], 80L),
      Condition = trialtracker:::fast_trunc(.data[["Condition"]], 30L),
      `Completion Date` = suppressWarnings(lubridate::ymd(.data[[
        "Completion Date"
      ]])),
      `Last Update Date` = suppressWarnings(lubridate::ymd(.data[[
        "Last Update Date"
      ]])),
      `Results Submit Date` = suppressWarnings(lubridate::ymd(.data[[
        "Results Submit Date"
      ]]))
    ) |>
    dplyr::select(dplyr::all_of(keep_cols))
}

#' Format recent changes: ISRCTN
#' @param df Data frame returned by the ISRCTN recent-changes SQL.
#' @return Formatted data frame.
#' @keywords internal
format_recent_changes_isrctn <- function(df) {
  keep_cols <- c(
    "Change",
    "Changed Field",
    "Program",
    "Guideline",
    "Date Added",
    "URL",
    "ISRCTN",
    "Acronym",
    "Title",
    "Recruitment Status",
    "Results URL",
    "Results Summary",
    "Results Completed",
    "Results Posted",
    "Results Published"
  )

  df |>
    dplyr::rename(Guideline = Guideline.number, ISRCTN = ISRCTN_No) |>
    dplyr::mutate(
      Title = trialtracker:::fast_trunc(.data[["Title"]], 75L),
      `Results Completed` = suppressWarnings(lubridate::dmy(.data[[
        "Results Completed"
      ]])),
      `Results Posted` = suppressWarnings(lubridate::dmy(.data[[
        "Results Posted"
      ]])),
      `Results Published` = suppressWarnings(lubridate::dmy(.data[[
        "Results Published"
      ]]))
    ) |>
    dplyr::select(dplyr::all_of(keep_cols))
}

#' Format recent changes: NIHR
#' @param df Data frame returned by the NIHR recent-changes SQL.
#' @return Formatted data frame.
#' @keywords internal
format_recent_changes_nihr <- function(df) {
  keep_cols <- c(
    "Change",
    "Changed Field",
    "Program",
    "Guideline",
    "Date Added",
    "URL",
    "NIHR ID",
    "Title",
    "Status",
    "End Date"
  )

  df |>
    dplyr::rename(Guideline = Guideline.number, `NIHR ID` = project_id) |>
    dplyr::mutate(Title = trialtracker:::fast_trunc(.data[["Title"]], 90L)) |>
    dplyr::select(dplyr::all_of(keep_cols))
}

#' Format recent changes: Clinicaltrials.eu
#' @param df Data frame returned by the EU recent-changes SQL.
#' @return Formatted data frame.
#' @keywords internal
format_recent_changes_eu <- function(df) {
  keep_cols <- c(
    "Change",
    "Changed Field",
    "Program",
    "Guideline",
    "Date Added",
    "URL",
    "EU ID",
    "End of Trial Status",
    "Acronym",
    "Title"
  )

  df |>
    dplyr::rename(Guideline = Guideline.number, `EU ID` = EU_Ids) |>
    dplyr::mutate(Title = trialtracker:::fast_trunc(.data[["Title"]], 90L)) |>
    dplyr::select(dplyr::all_of(keep_cols))
}
# ------------------------------------------------------------------------------
# Recent Status Changes: SQL builders (internal)
# ------------------------------------------------------------------------------

#' Build SQL for “Recent Status Changes” (NCT / ClinicalTrials.gov)
#'
#' Generates the SQL used by the dashboard to compare the latest snapshot vs an
#' older snapshot (lookback window) for tracked NCT IDs.
#'
#' @param con DBI connection (or pool) used for identifier quoting in glue_sql.
#' @param lookback_days Integer days to look back when selecting the “old” snapshot.
#' @return Character SQL string.
#' @keywords internal
nct_recent_changes_sql <- function(con, lookback_days = 31L) {
  glue::glue_sql(
    '
WITH
{trialtracker:::with_scaffold("NCT", "NCTId", "NCT_Ids", lookback_days = {lookback_days}, con = con)},
diffs AS (
  SELECT
    n."Program" AS "Program",
    n."Guideline.number" AS "Guideline.number",
    fs."Date Added",
    n."URL" AS "URL",
    n."Acronym" AS "Acronym",
    n."BriefTitle" AS "Title",
    n."Condition" AS "Condition",

    CASE WHEN n."OverallStatus"      IS NOT o."OverallStatus"
       AND (n."OverallStatus"      IS NOT NULL OR o."OverallStatus"      IS NOT NULL) THEN 1 ELSE 0 END AS "Status__changed",

    CASE WHEN n."CompletionDate"     IS NOT o."CompletionDate"
       AND (n."CompletionDate"     IS NOT NULL OR o."CompletionDate"     IS NOT NULL) THEN 1 ELSE 0 END AS "Completion Date__changed",

    CASE WHEN n."LastUpdatePostDate" IS NOT o."LastUpdatePostDate"
       AND (n."LastUpdatePostDate" IS NOT NULL OR o."LastUpdatePostDate" IS NOT NULL) THEN 1 ELSE 0 END AS "Last Update Date__changed",

    n."OverallStatus"            AS "Status",
    n."CompletionDate"           AS "Completion Date",
    n."ResultsFirstSubmitDate"   AS "Results Submit Date",
    n."LastUpdatePostDate"       AS "Last Update Date",
    n."NCTId"                    AS "NCTId"
  FROM n
  INNER JOIN o USING("NCTId")              -- strict baseline: o must exist
  LEFT JOIN first_seen fs USING("NCTId")
)
SELECT
  "Yes" AS "Change",
  RTRIM(
    (CASE WHEN "Status__changed" = 1 THEN "Status, " ELSE "" END) ||
    (CASE WHEN "Completion Date__changed" = 1 THEN "Completion Date, " ELSE "" END) ||
    (CASE WHEN "Last Update Date__changed" = 1 THEN "Last Update Date, " ELSE "" END),
    ", "
  ) AS "Changed Field",
  "Program","Guideline.number","Date Added","URL","NCTId","Acronym","Title","Condition",
  "Status","Completion Date","Results Submit Date","Last Update Date"
FROM diffs
WHERE ("Status__changed" + "Completion Date__changed" + "Last Update Date__changed") > 0
ORDER BY "Date Added" DESC, "NCTId";
',
    lookback_days = as.integer(lookback_days),
    .con = con
  ) |>
    as.character()
}


#' Build SQL for “Recent Status Changes” (ISRCTN)
#' @param con DBI connection (or pool) used for identifier quoting in glue_sql.
#' @param lookback_days Integer days to look back when selecting the “old” snapshot.
#' @return Character SQL string.
#' @keywords internal
isrctn_recent_changes_sql <- function(con, lookback_days = 31L) {
  glue::glue_sql(
    '
WITH
{trialtracker:::with_scaffold("ISRCTN", "ISRCTN_No", "ISRCTN_Ids", lookback_days = {lookback_days}, con = con)},
diffs AS (
  SELECT
    n."Program"          AS "Program",
    n."Guideline.number" AS "Guideline.number",
    fs."Date Added"      AS "Date Added",
    n."URL"              AS "URL",
    n."Acronym"          AS "Acronym",
    n."Scientific_Title" AS "Title",

    CASE WHEN n."Recruitment_Status" IS NOT o."Recruitment_Status"
      AND (n."Recruitment_Status" IS NOT NULL OR o."Recruitment_Status" IS NOT NULL) THEN 1 ELSE 0 END AS "Recruitment Status__changed",

    CASE WHEN n."Results_url_link" IS NOT o."Results_url_link"
      AND (n."Results_url_link" IS NOT NULL OR o."Results_url_link" IS NOT NULL) THEN 1 ELSE 0 END AS "Results URL__changed",

    CASE WHEN n."Results_summary" IS NOT o."Results_summary"
      AND (n."Results_summary" IS NOT NULL OR o."Results_summary" IS NOT NULL) THEN 1 ELSE 0 END AS "Results Summary__changed",

    CASE WHEN n."Results_date_completed" IS NOT o."Results_date_completed"
      AND (n."Results_date_completed" IS NOT NULL OR o."Results_date_completed" IS NOT NULL) THEN 1 ELSE 0 END AS "Results Completed__changed",

    CASE WHEN n."Results_date_posted" IS NOT o."Results_date_posted"
      AND (n."Results_date_posted" IS NOT NULL OR o."Results_date_posted" IS NOT NULL) THEN 1 ELSE 0 END AS "Results Posted__changed",

    CASE WHEN n."Results_date_first_publication" IS NOT o."Results_date_first_publication"
      AND (n."Results_date_first_publication" IS NOT NULL OR o."Results_date_first_publication" IS NOT NULL) THEN 1 ELSE 0 END AS "Results Published__changed",

    n."Recruitment_Status"               AS "Recruitment Status",
    n."Results_url_link"                 AS "Results URL",
    n."Results_summary"                  AS "Results Summary",
    n."Results_date_completed"           AS "Results Completed",
    n."Results_date_posted"              AS "Results Posted",
    n."Results_date_first_publication"   AS "Results Published",
    n."ISRCTN_No"                        AS "ISRCTN_No"

  FROM n
  INNER JOIN o USING("ISRCTN_No")            -- strict baseline
  LEFT JOIN first_seen fs USING("ISRCTN_No")
)
SELECT
  "Yes" AS "Change",
  RTRIM(
    (CASE WHEN "Recruitment Status__changed" = 1 THEN "Recruitment Status, " ELSE "" END) ||
    (CASE WHEN "Results URL__changed"        = 1 THEN "Results URL, "        ELSE "" END) ||
    (CASE WHEN "Results Summary__changed"    = 1 THEN "Results Summary, "    ELSE "" END) ||
    (CASE WHEN "Results Completed__changed"  = 1 THEN "Results Completed, "  ELSE "" END) ||
    (CASE WHEN "Results Posted__changed"     = 1 THEN "Results Posted, "     ELSE "" END) ||
    (CASE WHEN "Results Published__changed"  = 1 THEN "Results Published, "  ELSE "" END),
    ", "
  ) AS "Changed Field",
  "Program","Guideline.number","Date Added","URL","ISRCTN_No","Acronym","Title",
  "Recruitment Status","Results URL","Results Summary","Results Completed","Results Posted","Results Published"
FROM diffs
WHERE ("Recruitment Status__changed" + "Results URL__changed" + "Results Summary__changed" +
       "Results Completed__changed" + "Results Posted__changed" + "Results Published__changed") > 0
ORDER BY "Date Added" DESC, "ISRCTN_No";
',
    lookback_days = as.integer(lookback_days),
    .con = con
  ) |>
    as.character()
}


#' Build SQL for “Recent Status Changes” (NIHR)
#' @param con DBI connection (or pool) used for identifier quoting in glue_sql.
#' @param lookback_days Integer days to look back when selecting the “old” snapshot.
#' @return Character SQL string.
#' @keywords internal
nihr_recent_changes_sql <- function(con, lookback_days = 31L) {
  glue::glue_sql(
    '
WITH
{trialtracker:::with_scaffold("NIHR", "project_id", "NIHR_Ids", lookback_days = {lookback_days}, con = con)},
diffs AS (
  SELECT
    n."Program"          AS "Program",
    n."Guideline.number" AS "Guideline.number",
    fs."Date Added"      AS "Date Added",
    n."URL"              AS "URL",
    n."project_title"    AS "Title",

    CASE WHEN n."project_status" IS NOT o."project_status"
      AND (n."project_status" IS NOT NULL OR o."project_status" IS NOT NULL) THEN 1 ELSE 0 END AS "Status__changed",

    CASE WHEN n."end_date" IS NOT o."end_date"
      AND (n."end_date" IS NOT NULL OR o."end_date" IS NOT NULL) THEN 1 ELSE 0 END AS "End Date__changed",

    n."project_status" AS "Status",
    n."end_date"       AS "End Date",
    n."project_id"     AS "project_id"

  FROM n
  INNER JOIN o USING("project_id")          -- strict baseline
  LEFT JOIN first_seen fs USING("project_id")
)
SELECT
  "Yes" AS "Change",
  RTRIM(
    (CASE WHEN "Status__changed" = 1 THEN "Status, " ELSE "" END) ||
    (CASE WHEN "End Date__changed" = 1 THEN "End Date, " ELSE "" END),
    ", "
  ) AS "Changed Field",
  "Program","Guideline.number","Date Added","URL","project_id","Title","Status","End Date"
FROM diffs
WHERE ("Status__changed" + "End Date__changed") > 0
ORDER BY "Date Added" DESC, "project_id";
',
    lookback_days = as.integer(lookback_days),
    .con = con
  ) |>
    as.character()
}


#' Build SQL for “Recent Status Changes” (ClinicalTrialsEU)
#' @param con DBI connection (or pool) used for identifier quoting in glue_sql.
#' @param lookback_days Integer days to look back when selecting the “old” snapshot.
#' @return Character SQL string.
#' @keywords internal
eu_recent_changes_sql <- function(con, lookback_days = 31L) {
  glue::glue_sql(
    '
WITH
{trialtracker:::with_scaffold("EU", "EU_Ids", "EU_Ids", lookback_days = {lookback_days}, con = con)},
diffs AS (
  SELECT
    n."Program"          AS "Program",
    n."Guideline.number" AS "Guideline.number",
    fs."Date Added"      AS "Date Added",
    n."URL"              AS "URL",
    n."a32_name_or_abbreviated_title_of_the_trial_where_available" AS "Acronym",
    n."a3_full_title_of_the_trial" AS "Title",

    CASE WHEN n."p_end_of_trial_status" IS NOT o."p_end_of_trial_status"
      AND (n."p_end_of_trial_status" IS NOT NULL OR o."p_end_of_trial_status" IS NOT NULL) THEN 1 ELSE 0 END AS "End of Trial Status__changed",

    n."p_end_of_trial_status" AS "End of Trial Status",
    n."EU_Ids" AS "EU_Ids"

  FROM n
  INNER JOIN o USING("EU_Ids")              -- strict baseline
  LEFT JOIN first_seen fs USING("EU_Ids")
)
SELECT
  "Yes" AS "Change",
  RTRIM(
    (CASE WHEN "End of Trial Status__changed" = 1 THEN "End of Trial Status, " ELSE "" END),
    ", "
  ) AS "Changed Field",
  "Program","Guideline.number","Date Added","URL","EU_Ids","End of Trial Status","Acronym","Title"
FROM diffs
WHERE ("End of Trial Status__changed") > 0
ORDER BY "Date Added" DESC, "EU_Ids";
',
    lookback_days = as.integer(lookback_days),
    .con = con
  ) |>
    as.character()
}
