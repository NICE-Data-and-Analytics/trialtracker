#' Get Last Registry Entry Before Today
#'
#' This function retrieves the last entry date before today for a specified registry.
#'
#' @param registry A character string specifying the registry name.
#' @param main_con A database connection object.
#' @return The last entry date before today as a Date object.
#' @import DBI
#' @import dplyr
#' @export
get_last_registry_entry_before_today <- function(registry, main_con) {
  DBI::dbReadTable(main_con, registry) |>
    dplyr::filter(Query_Date < Sys.Date()) |>
    dplyr::pull(Query_Date) |>
    max() |>
    as.Date(origin = "1970-01-01")
}

#' Pull Change
#'
#' This function pulls changes from the registry table between specified dates.
#'
#' @param registry_table A character string specifying the registry table name.
#' @param main_con A database connection object.
#' @param start_date The start date for pulling changes.
#' @param end_date The end date for pulling changes.
#' @param group_cols A character vector of columns to group by. Defaults to NULL.
#' @param exclude_cols A character vector of columns to exclude. Defaults to NULL.
#' @param regex_pattern A regex pattern to filter Guideline numbers. Defaults to ".".
#' @return A comparison dataframe of changes.
#' @import DBI
#' @import dplyr
#' @import stringr
#' @import compareDF
#' @export
pull_change <- function(registry_table, main_con, start_date, end_date, group_cols = NULL, exclude_cols = NULL, regex_pattern = ".") {
  if (length(start_date) == 0) {
    df_old <- DBI::dbReadTable(main_con, registry_table)[0, ]
  } else {
    df_old <- DBI::dbReadTable(main_con, registry_table) |>
      dplyr::filter(
        Query_Date == start_date,
        stringr::str_detect(Guideline.number, regex_pattern)
      )
  }

  df_new <- DBI::dbReadTable(main_con, registry_table) |>
    dplyr::filter(
      Query_Date == end_date,
      stringr::str_detect(Guideline.number, regex_pattern)
    )

  compareDF::compare_df(
    df_new = df_new,
    df_old = df_old,
    group_col = group_cols,
    exclude = exclude_cols,
    stop_on_error = FALSE
  ) |>
    suppressWarnings()
}

#' Generate PubMed Dataframe for Email Attachment
#'
#' This function generates a PubMed dataframe for email attachments, based on specified dates.
#'
#' @param main_con A database connection object.
#' @param registry_table A character string specifying the registry table name.
#' @param start_date The start date for generating the dataframe.
#' @param end_date The end date for generating the dataframe.
#' @param regex_pattern A regex pattern to filter Guideline numbers. Defaults to ".".
#' @return A dataframe of PubMed results.
#' @import DBI
#' @import dplyr
#' @import stringr
#' @export
generate_pubmed_df_for_email_attachment <- function(main_con, registry_table, start_date, end_date, regex_pattern = ".") {
  DBI::dbReadTable(main_con, registry_table) |>
    dplyr::filter(
      Query_Date >= start_date & Query_Date <= end_date,
      stringr::str_detect(Guideline.number, regex_pattern)
    ) |>
    dplyr::mutate(Query_Date = as.Date(Query_Date, origin = "1970-01-01")) |>
    dplyr::distinct(doi, .keep_all = TRUE)
}

#' Write Changes to Disk
#'
#' This function writes changes to disk as an Excel file.
#'
#' @param Change_DF A comparison dataframe of changes.
#' @param daily_path A character string specifying the path to save the file.
#' @param DF_Name A character string specifying the name of the dataframe.
#' @param prog_name A character string specifying the program name. Defaults to "".
#' @return None. The function writes the changes to disk.
#' @import compareDF
#' @export
write_changes_to_disk <- function(Change_DF, daily_path, DF_Name, prog_name = "") {
  if (nrow(Change_DF$comparison_df) > 0) {
    compareDF::create_output_table(Change_DF,
                                   output_type = "xlsx",
                                   file_name = file.path(
                                     daily_path,
                                     paste0(
                                       DF_Name,
                                       "_",
                                       prog_name,
                                       "_Registry_Changes-",
                                       Sys.Date(),
                                       ".xlsx"
                                     )
                                   )
    )
  }
}

#' Write PubMed Dataframes to Disk
#'
#' This function writes PubMed dataframes to disk as CSV files.
#'
#' @param PM_DF A dataframe of PubMed results.
#' @param daily_path A character string specifying the path to save the file.
#' @param PM_DF_Name A character string specifying the name of the PubMed dataframe.
#' @return None. The function writes the PubMed dataframes to disk.
#' @import readr
#' @export
write_pubmed_dfs_to_disk <- function(PM_DF, daily_path, PM_DF_Name) {
  if (nrow(PM_DF) > 0) {
    readr::write_csv(PM_DF, file.path(
      daily_path,
      paste0(PM_DF_Name, "_Publications_", Sys.Date(), ".csv")
    ))
  }
}

#' Generate Trial Tracker Email
#'
#' This function generates an email to go out to users. Any changes found are detailed in attachments to the email.
#'
#' @param program A character string specifying the program name.
#' @param attachments A character vector of file paths to attach to the email.
#' @param dev_flag A logical flag indicating if the email is for the development version (dev team only).
#' @return An email object.
#' @import emayili
#' @export
generate_tt_email <- function(program, attachments, dev_flag,
                              users = readLines('secrets/users.csv'),
                              devs = readLines('secrets/devs.csv')) {

  email <- emayili::envelope() |>
    emayili::from("robert.willans@nice.org.uk")

  # If there are attachments to send
  if (length(attachments) > 0) {
    # Append Attachments
    for (i in seq_along(attachments)) {
      email <- email |>
        emayili::attachment(attachments[i])
    }

    # Amend email text and send
    email <- email |>
      emayili::to(if (dev_flag) {
        devs
      } else {
        users
      }) |>
      emayili::cc(if (dev_flag) {
        devs
      } else {
        devs
      }) |>
      emayili::subject(ifelse(dev_flag,
                              paste("Trial Tracking Changes", program, "DEV VERSION", sep = " - "), # Dev version subject
                              paste("Trial Tracking Changes", program, sep = " - ")
      )) |>
      emayili::text(ifelse(dev_flag,
                           paste("Trial Tracking Changes",
                                 program,
                                 Sys.Date(),
                                 "please email robert.willans@nice.org.uk with any queries",
                                 "DEV VERSION",
                                 sep = " - "
                           ),
                           paste("Trial Tracking Changes",
                                 program,
                                 Sys.Date(),
                                 "please email robert.willans@nice.org.uk with any queries",
                                 sep = " - "
                           )
      ))

    return(email)
  } else { # no attachments to send
    email <- email |>
      emayili::to(devs) |>
      emayili::subject(ifelse(dev_flag,
                              paste("TrialTracker ran today", program, "DEV VERSION", sep = " - "),
                              paste("TrialTracker ran today", program, sep = " - ")
      )) |>
      emayili::text(ifelse(dev_flag,
                           paste("TrialTracker script completed successfully today", Sys.Date(), "but noted no changes - DEV VERSION", sep = " - "),
                           paste("TrialTracker script completed successfully today", Sys.Date(), "but noted no changes")
      ))

    return(email)
  }
}

#' Generate All PubMed Dataframes for All Programs in One Registry
#'
#' This function generates PubMed dataframes for all programs in a specified registry.
#'
#' @param registry A character string specifying the registry name.
#' @param prog_regexes A character vector of regex patterns for programs.
#' @param programs A character vector of program names.
#' @param main_con A database connection object.
#' @return None. The function generates PubMed dataframes and writes them to disk.
#' @import purrr
#' @export
generate_all_pubmed_dfs_for_all_programs_one_registry <- function(registry, prog_regexes, programs, main_con) {
  purrr::walk2(
    .x = prog_regexes,
    .y = programs,
    ~ generate_pubmed_df_for_email_attachment(
      regex_pattern = .x,
      main_con = main_con,
      registry_table = paste0(registry, "_PM"),
      start_date = get_last_registry_entry_before_today(paste0(registry, "_PM"), main_con) + 1,
      end_date = Sys.Date()
    ) |>
      write_pubmed_dfs_to_disk(daily_path = daily_path, PM_DF_Name = paste0(registry, "_", .y))
  )
}

#' Pull Data for NIHR Comparison
#'
#' This function pulls data for NIHR comparison based on the specified regex pattern and date.
#'
#' @param prog_regex A regex pattern to filter Guideline numbers.
#' @param old_or_new A character string specifying whether to pull old or new data.
#' @param main_con A database connection object.
#' @return A dataframe of NIHR data.
#' @import DBI
#' @import dplyr
#' @import stringr
#' @export
pull_nihr_change <- function(prog_regex, old_or_new, main_con) {
  DBI::dbReadTable(main_con, "NIHR") |>
    dplyr::filter(
      Query_Date == if (old_or_new == "old") {
        get_last_registry_entry_before_today("NIHR", main_con)
      } else if (old_or_new == "new") {
        Sys.Date()
      },
      stringr::str_detect(Guideline.number, prog_regex)
    )
}

#' Generate NIHR Comparison Table
#'
#' This function generates a comparison table for a given program in the NIHR registry.
#'
#' @param program A character string specifying the program name.
#' @param program_regex A regex pattern to filter Guideline numbers.
#' @param main_con A database connection object.
#' @param daily_path A character string specifying the path to save the file.
#' @return None. The function generates a comparison table and writes it to disk.
#' @import compareDF
#' @export
generate_nihr_comparison_table <- function(program, program_regex, main_con, daily_path) {
  compareDF::compare_df(pull_nihr_change(prog_regex = program_regex, "new", main_con),
                        pull_nihr_change(prog_regex = program_regex, "old", main_con),
                        group_col = c("Guideline.number", "project_id"),
                        exclude = c("Query_Date", "URL", "project_title"),
                        stop_on_error = FALSE
  ) |>
    suppressWarnings() |>
    write_changes_to_disk(daily_path = daily_path, DF_Name = "NIHR", prog_name = program)
}

#' Generate All PubMed Dataframes for All Programs and Registries
#'
#' This function generates PubMed dataframes for all programs and registries.
#'
#' @param registry_tables A character vector of registry table names.
#' @param prog_regexes A character vector of regex patterns for programs.
#' @param programs A character vector of program names.
#' @param main_con A database connection object.
#' @return None. The function generates PubMed dataframes and writes them to disk.
#' @import purrr
#' @export
generate_all_pubmed_dfs_for_all_programs_and_registries <- function(registry_tables, prog_regexes, programs, main_con) {
  purrr::walk(registry_tables,
              generate_all_pubmed_dfs_for_all_programs_one_registry,
              prog_regexes = prog_regexes,
              programs = programs,
              main_con = main_con
  )
}

#' Generate Change Files for All Programs in One Registry
#'
#' This function generates change files for all programs in a specified registry.
#'
#' @param registry A character string specifying the registry name.
#' @param main_con A database connection object.
#' @param group_cols A character vector of columns to group by.
#' @param exclude_cols A character vector of columns to exclude.
#' @param prog_regexes A character vector of regex patterns for programs.
#' @param programs A character vector of program names.
#' @param daily_path A character string specifying the path to save the files.
#' @return None. The function generates change files and writes them to disk.
#' @import purrr
#' @export
generate_change_files_for_all_programs_in_one_registry <- function(registry,
                                                                   main_con,
                                                                   group_cols,
                                                                   exclude_cols,
                                                                   prog_regexes,
                                                                   programs,
                                                                   daily_path) {
  purrr::walk2(
    .x = prog_regexes,
    .y = programs,
    ~ pull_change(
      registry_table = registry,
      main_con = main_con,
      regex_pattern = .x,
      start_date = get_last_registry_entry_before_today(registry, main_con),
      end_date = Sys.Date(),
      group_cols = group_cols,
      exclude_cols = exclude_cols
    ) |>
      write_changes_to_disk(daily_path = daily_path, DF_Name = registry, prog_name = .y)
  )
}

#' Load Attachments and Send Email Alert for One Program
#'
#' This function attaches change files and sends an email alert for a specified program.
#'
#' @param program A character string specifying the program name.
#' @param daily_path A character string specifying the path to the attachment files.
#' @param dev_flag A logical flag indicating if the email is for development purposes.
#' @param smtp An SMTP server object.
#' @return None. The function sends an email alert with attachments.
#' @import stringr
#' @import emayili
#' @export
load_attachments_and_send_email_alert_for_one_program <- function(program, daily_path, dev_flag, smtp = smtp) {
  file_list <- dir(daily_path, full.names = TRUE) |> stringr::str_subset(paste0("_", program, "_"))

  generate_tt_email(program, file_list, dev_flag = dev_flag) |>
    smtp()
}

#' Load Attachments and Send Email Alert for All Programs
#'
#' This function attaches change files and sends email alerts for all specified programs.
#'
#' @param programs A character vector of program names.
#' @param daily_path A character string specifying the path to the attachment files.
#' @param dev_flag A logical flag indicating if the email is for development purposes.
#' @param smtp An SMTP server object.
#' @return None. The function sends email alerts with attachments.
#' @import purrr
#' @export
load_attachments_and_send_email_alert_for_all_programs <- function(programs = programs, daily_path, dev_flag, smtp) {
  purrr::walk(programs,
              load_attachments_and_send_email_alert_for_one_program,
              daily_path = daily_path,
              dev_flag = dev_flag,
              smtp = smtp
  )
}

#' Generate Change Files for NCT, ISRCTN, and EU Registries
#'
#' This function generates change files for NCT, ISRCTN, and EU registries.
#'
#' @param prog_regexes A character vector of regex patterns for programs.
#' @param programs A character vector of program names.
#' @param registry_tables A character vector of registry table names.
#' @param main_con A database connection object.
#' @param daily_path A character string specifying the path to save the files.
#' @return None. The function generates change files and writes them to disk.
#' @import purrr
#' @export
generate_change_files_for_nct_isrctn_eu <- function(prog_regexes, programs, registry_tables, main_con, daily_path) {
  if (length(registry_tables) == 0) {
    message("No registry tables provided.")
    return(NULL)
  }

  list(
    registry = as.list(registry_tables |> stringr::str_subset("NIHR", negate = TRUE)),
    group_cols = list(
      c("Guideline.number", "OrgStudyId"),
      c("Guideline.number", "ISRCTN_No"),
      c("Guideline.number", "X_id")
    ),
    exclude_cols = list(
      c("Query_Date", "Condition", "BriefTitle", "Acronym", "PrimaryCompletionDate", "ResultsFirstSubmitDate"),
      c("Query_Date", "Public_Title", "Acronym", "Scientific_Title", "URL"),
      c(
        "Query_Date", "a3_full_title_of_the_trial", "a32_name_or_abbreviated_title_of_the_trial_where_available",
        "a31_title_of_the_trial_for_lay_people_in_easily_understood_ie_nontechnical_language", "a41_sponsors_protocol_code_number"
      )
    )
  ) |>
    purrr::pwalk(generate_change_files_for_all_programs_in_one_registry, main_con = main_con, prog_regexes = prog_regexes, programs = programs,
                 daily_path = daily_path)
}


#' Generate Change Files for NIHR
#'
#' This function loops through programs and generates change files for the NIHR registry.
#'
#' @param prog_regexes A character vector of regex patterns for programs.
#' @param programs A character vector of program names.
#' @param main_con A database connection object.
#' @param daily_path A character string specifying the path to save the files.
#' @return None. The function generates change files and writes them to disk.
#' @import purrr
#' @export
generate_change_files_for_nihr <- function(prog_regexes, programs, main_con, daily_path) {
  purrr::walk2(
    .x = programs,
    .y = prog_regexes,
    generate_nihr_comparison_table,
    main_con = main_con,
    daily_path = daily_path
  )
}

#' Generate Email Alerts
#'
#' This function generates email alerts with attachments for trial tracking changes.
#'
#' @param dev_flag A logical flag indicating if the email is for development purposes.
#' @param base_path A character string specifying the base path for the daily directory.
#' @return None. The function generates and sends email alerts.
#' @import emayili
#' @import readr
#' @import DBI
#' @import RSQLite
#' @export
generate_email_alerts <- function(dev_flag, base_path = "data/email_attachments") {

  # Create daily directory to store attachment files
  daily_path <- file.path(base_path, Sys.Date(), "/")
  message("Attempting to create directory: ", daily_path)
  if (!dir.exists(daily_path)) {
    success <- dir.create(daily_path, recursive = TRUE)
    message("Directory creation success: ", success)
  }

  # SMTP settings
  smtp <- emayili::server(
    host = "smtp.mandrillapp.com",
    port = 587,
    username = "nice",
    password = readr::read_file("secrets/mandrill_pwd.txt")
  )

  # Setup con
  main_con <- DBI::dbConnect(RSQLite::SQLite(), "data/RSQLite_data/TrialTracker-db.sqlite")

  # List of programs (& regex matches)
  programs <- c("COVID", "IP", "Other")
  prog_regexes <- c("COVID", "IP[0-9]", "^(?!COVID|IP[0-9]).*") # last one is all but COVID|IP and is implied surveillance

  # List of tables to iterate over
  registry_tables <- c("NCT", "ISRCTN", "NIHR", "EU")

  # GENERATE NCT, ISRCTN AND EU CHANGE FILES
  generate_change_files_for_nct_isrctn_eu(
    prog_regexes = prog_regexes,
    programs = programs,
    registry_tables = registry_tables,
    main_con = main_con,
    daily_path = daily_path
  )

  # CREATE NIHR CHANGE FILES
  generate_change_files_for_nihr(prog_regexes = prog_regexes,
                                 programs = programs,
                                 main_con = main_con,
                                 daily_path = daily_path)

  # CREATE PUBMED CHANGE FILES
  generate_all_pubmed_dfs_for_all_programs_and_registries(
    registry_tables = registry_tables,
    prog_regexes = prog_regexes,
    programs = programs,
    main_con = main_con
  )
  # Close con
  DBI::dbDisconnect(main_con)

  # CREATE EMAILS, ATTACH FILES AND SEND EMAILS
  load_attachments_and_send_email_alert_for_all_programs(
    programs = programs,
    daily_path = daily_path,
    dev_flag = dev_flag,
    smtp = smtp
  )
}
