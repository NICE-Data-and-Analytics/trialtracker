get_last_registry_entry_before_today <- function(registry) {
  dbReadTable(con, registry) %>%
    select(Query_Date) %>%
    filter(Query_Date != Sys.Date()) %>%
    slice_max(order_by = Query_Date, n = 1, with_ties = FALSE) %>%
    simplify() %>%
    as.Date(origin = "1970-01-01") %>%
    unlist()
}
pull_change <- function(registry_table, con, start_date, end_date, group_cols = NULL, exclude_cols = NULL, regex_pattern = ".") {
  if (length(start_date) == 0) {
    df_old <- dbReadTable(con, registry_table)[0, ]
  } else {
    df_old <- dbReadTable(con, registry_table) %>% filter(
      Query_Date == start_date,
      str_detect(Guideline.number, regex_pattern)
    )
  }
  
  df_new <- dbReadTable(con, registry_table) %>% filter(
    Query_Date == end_date,
    str_detect(Guideline.number, regex_pattern)
  )
  
  compareDF::compare_df(
    df_new = df_new,
    df_old = df_old,
    group_col = group_cols,
    exclude = exclude_cols,
    stop_on_error = FALSE
  )
}
gen_pubmed_df <- function(con, registry_table, start_date, end_date, regex_pattern = ".") {
  dbReadTable(con = con, registry_table) %>%
    filter(
      Query_Date >= start_date & Query_Date <= end_date,
      str_detect(Guideline.number, regex_pattern)
    ) %>%
    mutate(Query_Date = as.Date(Query_Date, origin = "1970-01-01")) %>%
    distinct(doi, .keep_all = TRUE)
}
write_changes_to_disk <- function(Change_DF, DF_Name, prog_name = "") {
  if (nrow(Change_DF$comparison_df) > 0) {
    create_output_table(Change_DF,
                        output_type = "xlsx",
                        file_name = paste0(
                          daily_path,
                          DF_Name,
                          prog_name,
                          "_Registry_Changes-",
                          Sys.Date(),
                          ".xlsx"
                        )
    )
  }
}
write_PM_dfs_to_disk <- function(PM_DF, PM_DF_Name, prog_name = "") {
  if (nrow(PM_DF) > 0) {
    write_csv(
      PM_DF,
      paste0(
        daily_path,
        prog_name,
        PM_DF_Name,
        "_Publications_",
        Sys.Date(),
        ".csv"
      )
    )
  }
}