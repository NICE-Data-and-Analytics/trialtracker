% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/email_alert_functions.R
\name{pull_nihr_change}
\alias{pull_nihr_change}
\title{Pull Data for NIHR Comparison}
\usage{
pull_nihr_change(prog_regex, old_or_new, main_con)
}
\arguments{
\item{prog_regex}{A regex pattern to filter Guideline numbers.}

\item{old_or_new}{A character string specifying whether to pull old or new data.}

\item{main_con}{A database connection object.}
}
\value{
A dataframe of NIHR data.
}
\description{
This function pulls data for NIHR comparison based on the specified regex pattern and date.
}
