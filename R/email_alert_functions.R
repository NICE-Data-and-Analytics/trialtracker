get_last_registry_entry_before_today <- function(registry, con) {
  DBI::dbReadTable(con, registry) |>
    dplyr::filter(Query_Date != Sys.Date()) |>
    dplyr::pull(Query_Date) |>
    max() |>
    as.Date(origin = "1970-01-01")
}
pull_change <- function(registry_table, con, start_date, end_date, group_cols = NULL, exclude_cols = NULL, regex_pattern = ".") {
  if (length(start_date) == 0) {
    df_old <- DBI::dbReadTable(con, registry_table)[0, ]
  } else {
    df_old <- DBI::dbReadTable(con, registry_table) |>
      dplyr::filter(
        Query_Date == start_date,
        stringr::str_detect(Guideline.number, regex_pattern)
      )
  }

  df_new <- DBI::dbReadTable(con, registry_table) |>
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
  )
}
generate_pubmed_df_for_email_attachment <- function(con, registry_table, start_date, end_date, regex_pattern = ".") {
  DBI::dbReadTable(con = con, registry_table) |>
    dplyr::filter(
      Query_Date >= start_date & Query_Date <= end_date,
      stringr::str_detect(Guideline.number, regex_pattern)
    ) |>
    dplyr::mutate(Query_Date = as.Date(Query_Date, origin = "1970-01-01")) |>
    dplyr::distinct(doi, .keep_all = TRUE)
}
write_changes_to_disk <- function(Change_DF, daily_path, DF_Name, prog_name = "") {
  if (nrow(Change_DF$comparison_df) > 0) {
    compareDF::create_output_table(Change_DF,
      output_type = "xlsx",
      file_name = paste0(
        daily_path,
        DF_Name,
        "_",
        prog_name,
        "_Registry_Changes-",
        Sys.Date(),
        ".xlsx"
      )
    )
  }
}
write_pubmed_dfs_to_disk <- function(PM_DF, daily_path, PM_DF_Name) {
  if (nrow(PM_DF) > 0) {
    readr::write_csv(
      PM_DF,
      paste0(
        daily_path,
        PM_DF_Name,
        "_Publications_",
        Sys.Date(),
        ".csv"
      )
    )
  }
}
generate_tt_email <- function(program, attachments, dev_flag) {
  users <- c("niamh.knapton@nice.org.uk", "catherine.jacob@nice.org.uk")
  devs <- c("robert.willans@nice.org.uk", "jonathan.wray@nice.org.uk")

  email <- emayili::envelope() |>
    emayili::from("robert.willans@nice.org.uk")

  # If there are attachments to send
  if (length(attachments) > 0)

  # Append Attachments
    {
      for (i in seq_along(attachments)) {
        email <- email |>
          emayili::attachment(attachments[i])
      }

      # Amend email text and send
      email <- email |>
        emayili::to(if (dev_flag) {
          devs[1]
        } else {
          users
        }) |>
        emayili::cc(if (dev_flag) {
          devs[2]
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
    } else if (length(attachments) == 0) { # no attachments to send

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

# Functions with no unit tests

# Function to create pubmed files
generate_all_pubmed_dfs_for_all_programs_one_registry <- function(registry, prog_regexes, programs, con) {
  purrr::walk2(
    .x = prog_regexes,
    .y = programs,
    ~ generate_pubmed_df_for_email_attachment(
      regex_pattern = .x,
      con = con,
      registry_table = paste0(registry, "_PM"),
      start_date = get_last_registry_entry_before_today(paste0(registry, "_PM"), con) + 1,
      end_date = Sys.Date()
    ) |>
      write_pubmed_dfs_to_disk(daily_path = daily_path, PM_DF_Name = paste0(registry, "_", .y))
  )
}
# Function to pull_data_for_NIHR_comparison_data
pull_nihr_change <- function(prog_regex, old_or_new, con) {
  DBI::dbReadTable(con, "NIHR") |>
    dplyr::filter(
      Query_Date == if (old_or_new == "old") {
        get_last_registry_entry_before_today("NIHR", con)
      } else if (old_or_new == "new") {
        Sys.Date()
      },
      stringr::str_detect(Guideline.number, prog_regex)
    )
}
# Function to generate comparison dfs for given program
generate_nihr_comparison_table <- function(program, program_regex, con) {
  compareDF::compare_df(pull_nihr_change(prog_regex = program_regex, "new", con),
    pull_nihr_change(prog_regex = program_regex, "old", con),
    group_col = c("Guideline.number", "project_id"),
    exclude = c("Query_Date", "URL", "project_title"),
    stop_on_error = FALSE
  ) |>
    write_changes_to_disk(daily_path = daily_path, DF_Name = "NIHR", prog_name = program)
}
# Function to generate all pubmed dfs
generate_all_pubmed_dfs_for_all_programs_and_registries <- function(registry_tables, prog_regexes, programs) {
  # This maps over registries (outer loop) and programs (inner loop) to create pubmed changes files.
  # Inner loop is contained in anonymous function within within "generate_all_pubmed_dfs_for_all_programs_one_registry" function
  purrr::walk(registry_tables,
    generate_all_pubmed_dfs_for_all_programs_one_registry,
    prog_regexes = prog_regexes,
    programs = programs
  )
}
# Function to generate change files for one registry
generate_change_files_for_all_programs_in_one_registry <- function(registry, con, group_cols, exclude_cols, prog_regexes, programs) {
  purrr::walk2(
    .x = prog_regexes,
    .y = programs,
    ~ pull_change(
      registry_table = registry,
      con = con,
      regex_pattern = .x,
      start_date = get_last_registry_entry_before_today(registry, con),
      end_date = Sys.Date(),
      group_cols = group_cols,
      exclude_cols = exclude_cols
    ) |>
      write_changes_to_disk(daily_path = daily_path, DF_Name = registry, prog_name = .y)
  )
}
# Function to attach change file and send emails (one program)
load_attachments_and_send_email_alert_for_one_program <- function(program, daily_path, dev_flag, smtp = smtp) {
  file_list <- dir(daily_path, full.names = TRUE) |> stringr::str_subset(paste0("_", program, "_"))

  generate_tt_email(program, file_list, dev_flag = dev_flag) |>
    smtp()
}
# Function to attach change file and send emails (all programs)
load_attachments_and_send_email_alert_for_all_programs <- function(programs = programs, daily_path, dev_flag, smtp) {
  purrr::walk(programs,
    load_attachments_and_send_email_alert_for_one_program,
    daily_path = daily_path,
    dev_flag = dev_flag,
    smtp = smtp
  )
}
# Function to generate change files for NCT, ISRCTN, and EU registries
generate_change_files_for_nct_isrctn_eu <- function(prog_regexes, programs, registry_tables, con) {
  # Generate lists to map over
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
  ) |> # then pass to pwalk to generate the files
    purrr::pwalk(generate_change_files_for_all_programs_in_one_registry, con = con, prog_regexes = prog_regexes, programs = programs)
}
# Function to loop through programs generating NIHR change files
generate_change_files_for_nihr <- function(prog_regexes, programs, con) {
  purrr::walk2(
    .x = programs,
    .y = prog_regexes,
    generate_nihr_comparison_table,
    con = con
  )
}
generate_email_alerts <- function(dev_flag) {
  # Add path into config file or somesuch

  # Create daily directory to store attachment files
  daily_path <- paste0("Email_Attachments", "/", Sys.Date(), "/")
  if (!dir.exists(daily_path)) {
    dir.create(daily_path)
  }

  # Setup con
  con <- DBI::dbConnect(RSQLite::SQLite(), "RSQLite_Data/TrialTracker-db.sqlite")

  # setup smtp settings
  smtp <- emayili::server(
    host = "smtp.mandrillapp.com",
    port = 587,
    username = "nice",
    password = readr::read_file("Secrets/mandrill_pwd.txt")
  )

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
    con = con
  )

  # CREATE NIHR CHANGE FILES

  generate_change_files_for_nihr(prog_regexes, programs, con)

  # CREATE PUBMED CHANGE FILES

  generate_all_pubmed_dfs_for_all_programs_and_registries(
    registry_tables = registry_tables,
    prog_regexes = prog_regexes,
    programs = programs
  )
  # Close con
  DBI::dbDisconnect(con)

  # CREATE EMAILS, ATTACH FILES AND SEND EMAILS

  load_attachments_and_send_email_alert_for_all_programs(
    programs = programs,
    daily_path = daily_path,
    dev_flag = dev_flag,
    smtp = smtp
  )
}
