#' Add Program Column to Tibble
#'
#' This function adds a "Program" column to a tibble.
#'
#' @param tibble A tibble dataframe.
#' @return The tibble with an added "Program" column.
#' @importFrom tibble add_column
add_program_col_to_tibble <- function(tibble) {
  tibble |>
    tibble::add_column("Program" = NA, .before = "Guideline.number")
}

#' Add Program Column to SQL Table
#'
#' This function adds a "Program" column to a SQL table.
#'
#' @param table A character string specifying the table name.
#' @param main_con A database connection object.
#' @return None. The function updates the SQL table with an added "Program" column.
#' @importFrom DBI dbReadTable dbWriteTable
add_program_col_to_sql_table <- function(table, main_con) {
  tibble <- DBI::dbReadTable(main_con, table) |>
    add_program_col_to_tibble()

  DBI::dbWriteTable(main_con, name = table, value = tibble, overwrite = TRUE)
}

#' Write Program Column to DataFrame
#'
#' This function writes the "Program" column to a dataframe based on the "Guideline.number".
#'
#' @param sql_table A character string specifying the SQL table name.
#' @param main_con A database connection object.
#' @return None. The function updates the SQL table with the "Program" column.
#' @importFrom DBI dbReadTable dbWriteTable
#' @importFrom dplyr mutate case_when
#' @importFrom stringr str_detect
write_prog_col_to_df <- function(sql_table, main_con) {
  df <- DBI::dbReadTable(main_con, sql_table)

  df <- df |>
    dplyr::mutate("Program" = dplyr::case_when(
      Guideline.number == "COVID" ~ "COVID",
      Guideline.number == "NG191" ~ "COVID",
      Guideline.number == "NG188" ~ "COVID",
      stringr::str_detect(Guideline.number, "IPG[0-9]*") ~ "Other",
      stringr::str_detect(Guideline.number, "IP[0-9]*") ~ "IP",
      TRUE ~ "Other"
    ))

  DBI::dbWriteTable(main_con, sql_table, df, overwrite = TRUE)
}

#' Return Only Distinct Rows
#'
#' This function removes duplicates from a single table and returns only distinct rows.
#'
#' @param main_con A database connection object.
#' @param table_name A character string specifying the table name.
#' @return A dataframe with only distinct rows.
#' @importFrom DBI dbReadTable
#' @importFrom dplyr distinct
return_only_distinct <- function(main_con, table_name) {
  DBI::dbReadTable(main_con, table_name) |> dplyr::distinct()
}

#' Remove Duplicates and Overwrite Table
#'
#' This function removes duplicates from a table and writes back the table, overwriting the original.
#'
#' @param table_name A character string specifying the table name.
#' @param main_con A database connection object.
#' @return None. The function updates the SQL table by removing duplicates.
#' @importFrom DBI dbWriteTable
remove_dups_and_overwrite_table <- function(table_name, main_con) {
  clean <- return_only_distinct(main_con = main_con, table_name = table_name)

  DBI::dbWriteTable(main_con, name = table_name, value = clean, overwrite = TRUE)
}

#' Rename Program "NG" to "Other"
#'
#' This function renames all instances of Program "NG" to "Other" in a specified table.
#'
#' @param table_name A character string specifying the table name.
#' @param main_con A database connection object.
#' @return None. The function updates the SQL table by renaming "NG" to "Other".
#' @importFrom DBI dbReadTable dbWriteTable
#' @importFrom dplyr mutate case_when
rename_ng_to_other <- function(table_name, main_con) {
  tab <- DBI::dbReadTable(main_con, name = table_name)

  tab_new <- tab |> dplyr::mutate("Program" = dplyr::case_when(
    Program == "NG" ~ "Other",
    TRUE ~ Program
  ))

  DBI::dbWriteTable(main_con, name = table_name, value = tab_new, overwrite = TRUE)
}

