#' Create a Monthly Backup of SQLite Database Files
#'
#' This function creates a monthly backup of the SQLite database files. It ensures that a directory for storing monthly backups exists, creates a subdirectory for the current month, and copies the SQLite files to this subdirectory.
#'
#' @param backup_subdirectory_path A character string specifying the path to the directory where monthly backups will be stored. Defaults to "data/monthly_SQLite_backup".
#' @return None. The function is called for its side effects.
#' @examples
#' backup_SQLite_db()
#' backup_SQLite_db("custom/path/to/backup")
#' @export
backup_SQLite_db <- function(backup_subdirectory_path = "data/monthly_SQLite_backup") {
  # Create a directory to store monthly SQLite backups
  # if it doesn't already exist
  if (!dir.exists(backup_subdirectory_path)) {
    dir.create(backup_subdirectory_path, recursive = TRUE)
  }

  # Set the current monthly path
  monthly_path <- file.path("data",
                            "monthly_SQLite_backup",
                            stringr::str_sub(as.character(Sys.Date()), 1, 7)
  )

  # Create a directory to store this month's SQLite backup
  # if it doesn't already exist
  if (!dir.exists(monthly_path)) {
    dir.create(monthly_path)
  }

  # Copy SQLite files to backup folder

  # Get the paths for all files in the RSQLite_Data folder
  files_to_copy <- dir(file.path("data/RSQLite_data"), full.names = TRUE)

  # Copy the files to the new folder
  file.copy(files_to_copy, monthly_path, overwrite = TRUE)
}
