#' Create a Monthly Backup of SQLite Database Files
#'
#' This function creates a monthly backup of the SQLite database files. It ensures that a directory for storing monthly backups exists, creates a subdirectory for the current month, and copies the SQLite files to this subdirectory.
#'
#' @return None. The function is called for its side effects.
#' @examples
#' backup_SQLite_db()
#' @export
backup_SQLite_db <- function() {
  # Create a directory to store monthly SQLite backups
  # if it doesn't already exist
  if (!dir.exists("data/monthly_SQLite_backup")) {
    dir.create("data/Monthly_SQLite_backup", recursive = TRUE)
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

# Function to create a monthly backup of the SQLlite database files

backup_SQLite_db <- function() {
  # Create a directory to store monthly SQLlite backups
  # if it doesn't already exist
  if (!dir.exists("data/monthly_SQLite_backup")) {
    dir.create("data/Monthly_SQLite_backup", recursive = TRUE)
  }

  # Set the current monthly path
  monthly_path <- file.path("data",
    "monthly_SQLite_backup",
    stringr::str_sub(as.character(Sys.Date()), 1, 7)
  )

  # Create a directory to store this months SQLlite backup
  # if it doesn't already exist
  if (!dir.exists(monthly_path)) {
    dir.create(monthly_path)
  }

  # Copy SQLlite files to backup folder

  # Get the paths for all files in the RSQLite_Data folder
  files_to_copy <- dir(file.path("data/RSQLite_data"), full.names = TRUE)

  # Copy the files to the new folder
  file.copy(files_to_copy, monthly_path, overwrite = TRUE)
}
