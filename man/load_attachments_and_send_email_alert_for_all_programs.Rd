% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/email_alert_functions.R
\name{load_attachments_and_send_email_alert_for_all_programs}
\alias{load_attachments_and_send_email_alert_for_all_programs}
\title{Load Attachments and Send Email Alert for All Programs}
\usage{
load_attachments_and_send_email_alert_for_all_programs(
  programs = programs,
  daily_path,
  dev_flag,
  smtp
)
}
\arguments{
\item{programs}{A character vector of program names.}

\item{daily_path}{A character string specifying the path to the attachment files.}

\item{dev_flag}{A logical flag indicating if the email is for development purposes.}

\item{smtp}{An SMTP server object.}
}
\value{
None. The function sends email alerts with attachments.
}
\description{
This function attaches change files and sends email alerts for all specified programs.
}
