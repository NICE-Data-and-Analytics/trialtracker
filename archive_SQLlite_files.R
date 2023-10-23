
# File to create a monthly backup of the SQLlite database files 

# Load packages and setup-------------------------------------------------------

library(tidyverse)
library(DBI)

# Use the here package to set the relative file path
here::i_am("trialtracker.Rproj")

# Create a directory to store monthly SQLlite backups if it doesn't already exist
if (!dir.exists(here::here("Monthly_SQLlite_backup"))) {
  dir.create(here::here("Monthly_SQLlite_backup"))
}

# Set the current monthly path
monthly_path <- here::here("Monthly_SQLlite_backup", 
                           str_sub(as.character(Sys.Date()), 1, 7))

# Create a directory to store this months SQLlite backup if it doesn't already exist
if (!dir.exists(monthly_path)) {
  dir.create(monthly_path)
}

# Copy SQLlite files to backup folder -------------------------------------

# Get the paths for all files in the RSQLite_Data folder
# Do we add all.files = TRUE? This copies hidden files, and returns two additional files named 
# "." and "..". What are these files? Do we need them?
files_to_copy <- dir(here::here("RSQLite_Data"), full.names = TRUE, all.files = TRUE)

# COpy the files to the new folder
file.copy(files_to_copy, monthly_path, overwrite = TRUE)
