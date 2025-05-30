% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_trial_info_functions.R
\name{update_db_for_NIHR_changes}
\alias{update_db_for_NIHR_changes}
\title{Update Database for NIHR Registry Changes}
\usage{
update_db_for_NIHR_changes(main_con, trial_id_df)
}
\arguments{
\item{main_con}{A database connection object.}

\item{trial_id_df}{A dataframe containing trial IDs.}
}
\value{
None. The function updates the NIHR registry data in the database.
}
\description{
This function updates the database for changes in the NIHR registry.
}
