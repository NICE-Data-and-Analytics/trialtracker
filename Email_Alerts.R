# Email Alerts for changes and pubmed info

#options echo true
options(echo = TRUE)

# Library
library(tidyverse)
library(DBI)
library(compareDF)
library(emayili)
library(here)

# Setup con
con <- dbConnect(RSQLite::SQLite(), here("RSQLite_Data/TrialTracker-db.sqlite"))

### FUNCTIONS
get_last_registry_entry_before_today <- function(registry) {
  dbReadTable(con, registry) %>%
    select(Query_Date) %>%
    filter(Query_Date != Sys.Date()) %>%
    slice_max(order_by = Query_Date, n = 1, with_ties = FALSE) %>% 
    simplify() %>% 
    as.Date(origin = '1970-01-01')
}
pull_change <- function(con, registry_table, start_date, end_date, group_cols = NULL, exclude_cols = NULL) {
  
  if (length(start_date) == 0) {
    df_old <- dbReadTable(con, registry_table)[0,]
  } else {
    df_old <- dbReadTable(con, registry_table) %>% filter(Query_Date == start_date)  
  }
  
  df_new <- dbReadTable(con, registry_table) %>% filter(Query_Date == end_date)
  
  compareDF::compare_df(df_new = df_new,
                        df_old = df_old,
                        group_col = group_cols,
                        exclude = exclude_cols,
                        stop_on_error = FALSE
  )
  
}
gen_pubmed_df <- function(con, registry_table, start_date, end_date) {
  
  dbReadTable(con = con, registry_table) %>% 
    filter(Query_Date >= start_date & Query_Date <= end_date) %>% 
    mutate(Query_Date = as.Date(Query_Date, origin = "1970-01-01"))
  
}
write_changes_to_disk <- function(Change_DF, DF_Name) {
  
  if (nrow(Change_DF$comparison_df) > 0 ) {
    
    create_output_table(Change_DF,
                        output_type = "xlsx",
                        file_name = here(daily_path,
                                         paste0(DF_Name, "_Registry_Changes-", Sys.Date(), ".xlsx"))
    )
  }
}
write_PM_dfs_to_disk <- function(PM_DF, PM_DF_Name) {
  
  if (nrow(PM_DF) > 0 ) {
    write_csv(PM_DF,
              here(daily_path,
                   paste0(PM_DF_Name, "_Publications_", Sys.Date(), ".csv")
              )
    )
  }
}

#Create daily_path
daily_path <- here("Email_Attachments", Sys.Date())
if (!dir.exists(daily_path)) {
  dir.create(daily_path)
}

# Generate comparison dfs
NCT_Changes <- 
  pull_change(con = con,
              registry_table = "NCT",
              start_date = get_last_registry_entry_before_today("NCT"),
              end_date = Sys.Date(),
              group_cols = c('Guideline.number', 'OrgStudyId'),
              exclude_cols = c("Query_Date", "Condition", "BriefTitle", "Acronym",
                               "PrimaryCompletionDate", "ResultsFirstSubmitDate")
  )

ISRCTN_Changes <-
  pull_change(con = con,
              registry_table = "ISRCTN",
              start_date = get_last_registry_entry_before_today("ISRCTN"),
              end_date = Sys.Date(),
              group_cols = c("Guideline.number", "ISRCTN_No"),
              exclude_cols = c("Query_Date", "Public_Title", "Acronym", "Scientific_Title", "URL")
  )

nihr_old <- dbReadTable(con, "NIHR") %>% filter(Query_Date == get_last_registry_entry_before_today("NIHR"))
nihr_new <- dbReadTable(con, "NIHR") %>% filter(Query_Date == Sys.Date())

NIHR_Changes <- compare_df(nihr_new,
                           nihr_old,
                           group_col = c("Guideline.number", "project_id"),
                           exclude = c("Query_Date", "URL", "project_title"),
                           stop_on_error = FALSE
)

EU_Changes <-
  pull_change(con = con,
              registry_table = "EU",
              start_date = get_last_registry_entry_before_today("EU"),
              end_date = Sys.Date(),
              group_cols = c('Guideline.number', 'X_id'),
              exclude_cols = c(
                "Query_Date",
                "a3_full_title_of_the_trial",
                "a32_name_or_abbreviated_title_of_the_trial_where_available",
                "a31_title_of_the_trial_for_lay_people_in_easily_understood_ie_nontechnical_language",
                "a41_sponsors_protocol_code_number")
  )

# Pubmed queries for previous days
if ("NCT_PM" %in% dbListTables(con)) {
  NCT_PM <- gen_pubmed_df(con, "NCT_PM", get_last_registry_entry_before_today("NCT_PM") + 1, Sys.Date())
}
if ("ISRCTN_PM" %in% dbListTables(con)) {
  ISRCTN_PM <- gen_pubmed_df(con, "ISRCTN_PM", get_last_registry_entry_before_today("ISRCTN_PM") + 1, Sys.Date())
}
if ("NIHR_PM" %in% dbListTables(con)) {
  NIHR_PM <- gen_pubmed_df(con, "NIHR_PM", get_last_registry_entry_before_today("NIHR_PM") + 1, Sys.Date())
}
if ("EU_PM" %in% dbListTables(con)) {
  EU_PM <- gen_pubmed_df(con, "EU_PM", get_last_registry_entry_before_today("EU_PM") + 1, Sys.Date())
}

#create attachments
write_changes_to_disk(NCT_Changes, "NCT")
write_changes_to_disk(ISRCTN_Changes, "ISRCTN")
write_changes_to_disk(NIHR_Changes, "NIHR")
write_changes_to_disk(EU_Changes, "EU")

write_PM_dfs_to_disk(NCT_PM, "NCT")
write_PM_dfs_to_disk(ISRCTN_PM, "ISRCTN")
write_PM_dfs_to_disk(NIHR_PM, "NIHR")
write_PM_dfs_to_disk(EU_PM, "EU")

# Close con
dbDisconnect(con)

# create email, attach files and send
email <- envelope() %>%
  from("robert.willans@nice.org.uk") %>%
  to("niamh.knapton@nice.org.uk") %>%
  cc("catherine.jacob@nice.org.uk",
	"robert.willans@nice.org.uk") %>%
  text(
    paste("Trial Tracking Changes",
          Sys.Date(),
          "Note that this email inbox is not monitored - please email robert.willans@nice.org.uk with any queries",
          sep = " - ")
  ) %>%
  subject(subject = "Trial Tracking Changes")

files <- dir(daily_path, full.names = TRUE)

if (length(files) > 0) {
  for (i in 1:length(files)) {
    email <- email %>% 
      attachment(files[i])
  }  
}

if (length(email$parts) > 1) {
  smtp <- server(host = "smtp.mandrillapp.com",
                 port = 587,
                 username = "nice",
                 password = read_file(here("Data_Files", "mandrill_pwd.txt")))
  
  smtp(email, verbose = FALSE)
  
} else {
  email <- envelope() %>% 
    from("robert.willans@nice.org.uk") %>% 
    to("robert.willans@nice.org.uk") %>% 
    subject("TrialTracker ran today") %>% 
    text(paste0("TrialTracker script completed successfully today (", Sys.Date(), ") but noted no changes"))
  
  smtp <- server(host = "smtp.mandrillapp.com",
                 port = 587,
                 username = "nice",
                 password = read_file(here("Data_Files", "mandrill_pwd.txt")))
  
  smtp(email, verbose = FALSE)
}