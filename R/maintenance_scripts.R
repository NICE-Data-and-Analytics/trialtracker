## Functions to add Program
add_program_col_to_tibble <- function(tibble) {
  tibble |>
    tibble::add_column("Program" = NA, .before = "Guideline.number")
}

add_program_col_to_sql_table <- function(table, con) {
  tibble <- DBI::dbReadTable(con, table) |>
    add_program_col_to_tibble()

  DBI::dbWriteTable(conn = con, name = table, value = tibble, overwrite = TRUE)
}

write_prog_col_to_df <- function(sql_table) {
  df <- DBI::dbReadTable(con, sql_table)

  df <- df |>
    dplyr::mutate("Program" = dplyr::case_when(
      Guideline.number == "COVID" ~ "COVID",
      Guideline.number == "NG191" ~ "COVID",
      Guideline.number == "NG188" ~ "COVID",
      str_detect(Guideline.number, "IPG[0-9]*") ~ "Other",
      str_detect(Guideline.number, "IP[0-9]*") ~ "IP",
      TRUE ~ "Other"
    ))

  DBI::dbWriteTable(con, sql_table, df, overwrite = TRUE)
}

# Function to remove duplicates from single table
return_only_distinct <- function(con, table_name) {
  DBI::dbReadTable(con, table_name) |> dplyr::distinct()
}

# Function to remove duplicates and write back table (overwrite)
remove_dups_and_overwrite_table <- function(table_name, con) {
  clean <- return_only_distinct(con = con, table_name = table_name)

  DBI::dbWriteTable(conn = con, name = table_name, value = clean, overwrite = TRUE)
}

# Function to switch all program = "NG" to program = "other"
rename_ng_to_other <- function(table_name, con) {
  tab <- DBI::dbReadTable(conn = con, name = table_name)

  tab_new <- tab |> dplyr::mutate("Program" = dplyr::case_when(
    Program == "NG" ~ "Other",
    TRUE ~ Program
  ))

  DBI::dbWriteTable(conn = con, name = table_name, value = tab_new, overwrite = TRUE)
}
