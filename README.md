# trialtracker

This is the code for the internal TrialTracker dashboard used by IS to monitor trials of interest and identify results when published.

**Installation**

To generate the *directory structure*, download or clone the repo via the usual method. You may have to recreate / copy the secrets folder (api keys and passwords) and the data folder (SQLite databases and email attachments).

To install the *package* and provide the functions install from the repo via `install.packages("NICE-Data-and-Analytics/trialtracker")`. Load the package in via the usual `library(trialtracker)` or similar function call.

## Folder Structure

**Flexdashboard.rmd** - Is the flexdashboard file which is the dashboard front-end
**data** - folder that holds the underlying SQLite databases for the various registries and pubmed (in subdirectory RSQLite_data), the generated email attachments (in subdirectory email_attachments), and the monthly backup (subdirectory monthly_SQLite_backup)
**man** - documentation for each of the functions
**R** - files containing the functions
**secrets** - api keys and passwords. You may need to manually recreate these as they will not be in the github repo
**tests** - unit tests created with `testthat`

## Operation

The main wrapper function `download_trial_info_wrapper()` calls subsequent functions to update the database and generate email alerts as appropriate. This function, with the 'dev_flag' set appropriately to TRUE or FALSE, should be scheduled as a cron job on the hosting instance.
Further information can be found in the function documentation.
