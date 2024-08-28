# File to create a monthly backup of the SQLlite database files 

# Create a directory to store monthly SQLlite backups if it doesn't already exist
if (!dir.exists("Monthly_SQLite_backup")) {
  dir.create("Monthly_SQLite_backup")
}

# Set the current monthly path
monthly_path <- file.path(getwd(), "Monthly_SQLite_backup", 
                           stringr::str_sub(as.character(Sys.Date()), 1, 7))

# Create a directory to store this months SQLlite backup if it doesn't already exist
if (!dir.exists(monthly_path)) {
  dir.create(monthly_path)
}

# Copy SQLlite files to backup folder -------------------------------------

# Get the paths for all files in the RSQLite_Data folder
files_to_copy <- dir(file.path(getwd(), "RSQLite_Data"), full.names = TRUE)

# Copy the files to the new folder
file.copy(files_to_copy, monthly_path, overwrite = TRUE)
