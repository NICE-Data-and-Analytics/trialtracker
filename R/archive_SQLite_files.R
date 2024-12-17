#' Create a Monthly Backup of SQLite Database Files
#'
#' This function creates a monthly backup of the SQLite database files. It ensures that a directory for storing monthly backups exists, creates a subdirectory for the current month, and copies the SQLite files to this subdirectory.
#'
#' @param backup_subdirectory_path A character string specifying the path to the directory where monthly backups will be stored. Defaults to "inst/extdata/monthly_SQLite_backup".
#' @return None. The function is called for its side effects.
#' @examples
#' backup_SQLite_db()
#' backup_SQLite_db("custom/path/to/backup")
#' @export
backup_SQLite_db <- function(backup_subdirectory_path = "inst/extdata/monthly_SQLite_backup") {
  # Normalize the backup subdirectory path
  backup_subdirectory_path <- normalizePath(backup_subdirectory_path, winslash = "/", mustWork = FALSE)
  message("Normalized backup subdirectory path: ", backup_subdirectory_path)

  # Create a directory to store monthly SQLite backups if it doesn't already exist
  if (!dir.exists(backup_subdirectory_path)) {
    message("Creating backup subdirectory: ", backup_subdirectory_path)
    dir.create(backup_subdirectory_path, recursive = TRUE)
    if (!dir.exists(backup_subdirectory_path)) {
      stop("Failed to create backup subdirectory: ", backup_subdirectory_path)
    }
  }

  # Set the current monthly path
  monthly_path <- file.path(backup_subdirectory_path, stringr::str_sub(as.character(Sys.Date()), 1, 7))
  monthly_path <- normalizePath(monthly_path, winslash = "/", mustWork = FALSE)
  message("Monthly path: ", monthly_path)

  # Create a directory to store this month's SQLite backup if it doesn't already exist
  if (!dir.exists(monthly_path)) {
    message("Creating monthly backup directory: ", monthly_path)
    dir.create(monthly_path)
    if (!dir.exists(monthly_path)) {
      stop("Failed to create monthly backup directory: ", monthly_path)
    }
  }

  # Copy SQLite files to backup folder
  # Get the paths for all files in the RSQLite_Data folder
  files_to_copy <- dir(file.path("inst/extdata/RSQLite_data"), full.names = TRUE)
  message("Files to copy: ", paste(files_to_copy, collapse = ", "))

  # Copy the files to the new folder
  file.copy(files_to_copy, monthly_path, overwrite = TRUE)
}
