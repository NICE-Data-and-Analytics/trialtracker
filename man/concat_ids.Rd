% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_trial_info_functions.R
\name{concat_ids}
\alias{concat_ids}
\title{Concatenate Trial IDs for API URL Construction}
\usage{
concat_ids(df, id_col)
}
\arguments{
\item{df}{A dataframe containing the IDs.}

\item{id_col}{A string specifying the column name that contains the IDs.}
}
\value{
A vector of unique, non-missing IDs.
}
\description{
This internal function concatenates Trial IDs from a specified column in a dataframe, ensuring no missing or duplicate values. It is used for constructing API URLs.
}
\keyword{internal}
