% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maintenance_scripts.R
\name{return_only_distinct}
\alias{return_only_distinct}
\title{Return Only Distinct Rows}
\usage{
return_only_distinct(main_con, table_name)
}
\arguments{
\item{main_con}{A database connection object.}

\item{table_name}{A character string specifying the table name.}
}
\value{
A dataframe with only distinct rows.
}
\description{
This function removes duplicates from a single table and returns only distinct rows.
}
