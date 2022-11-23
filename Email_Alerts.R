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
    as.Date(origin = '1970-01-01') %>% 
    unlist()
}
pull_change <- function(registry_table, con, start_date, end_date, group_cols = NULL, exclude_cols = NULL, regex_pattern = ".") {
  
  if (length(start_date) == 0) {
    df_old <- dbReadTable(con, registry_table)[0,]
  } else {
    df_old <- dbReadTable(con, registry_table) %>% filter(Query_Date == start_date,
                                                          str_detect(Guideline.number, regex_pattern))  
  }
  
  df_new <- dbReadTable(con, registry_table) %>% filter(Query_Date == end_date,
                                                        str_detect(Guideline.number, regex_pattern))
  
  compareDF::compare_df(df_new = df_new,
                        df_old = df_old,
                        group_col = group_cols,
                        exclude = exclude_cols,
                        stop_on_error = FALSE
  )
  
}
gen_pubmed_df <- function(con, registry_table, start_date, end_date, regex_pattern = ".") {
  
  dbReadTable(con = con, registry_table) %>% 
    filter(Query_Date >= start_date & Query_Date <= end_date,
           str_detect(Guideline.number, regex_pattern)) %>% 
    mutate(Query_Date = as.Date(Query_Date, origin = "1970-01-01"))
  
}
write_changes_to_disk <- function(Change_DF, DF_Name, prog_name = "") {
  
  if (nrow(Change_DF$comparison_df) > 0 ) {
    
    create_output_table(Change_DF,
                        output_type = "xlsx",
                        file_name = here(daily_path,
                                         paste0(DF_Name,
                                                prog_name,
                                                "_Registry_Changes-",
                                                Sys.Date(),
                                                ".xlsx"))
    )
  }
}
write_PM_dfs_to_disk <- function(PM_DF, PM_DF_Name, prog_name = "") {
  
  if (nrow(PM_DF) > 0 ) {
    write_csv(PM_DF,
              here(daily_path,
                   prog_name, 
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

#List of programs (regex matches)
prog_regexes <- c('COVID', 'IP', "^(?!COVID|IP).*") # last one is all but COVID|IP and is implied surveillance

#List of tables to iterate over
registry_tables <- c("NCT", "ISRCTN", "NIHR", "EU")

# Generate comparison dfs
NCT_Changes_COVID <- 
  pull_change(con = con,
              registry_table = "NCT",
              regex_pattern = prog_regexes[1],
              start_date = get_last_registry_entry_before_today("NCT"),
                            end_date = Sys.Date(),
              group_cols = c('Guideline.number', 'OrgStudyId'),
              exclude_cols = c("Query_Date", "Condition", "BriefTitle", "Acronym",
                               "PrimaryCompletionDate", "ResultsFirstSubmitDate")
  )


NCT_Changes_IP <- 
  pull_change(con = con,
              registry_table = "NCT",
              regex_pattern = prog_regexes[2],
              start_date = get_last_registry_entry_before_today("NCT"),
              end_date = Sys.Date(),
              group_cols = c('Guideline.number', 'OrgStudyId'),
              exclude_cols = c("Query_Date", "Condition", "BriefTitle", "Acronym",
                               "PrimaryCompletionDate", "ResultsFirstSubmitDate")
  )

NCT_Changes_Surv_Oth <- 
  pull_change(con = con,
              registry_table = "NCT",
              regex_pattern = prog_regexes[3],
              start_date = get_last_registry_entry_before_today("NCT"),
              end_date = Sys.Date(),
              group_cols = c('Guideline.number', 'OrgStudyId'),
              exclude_cols = c("Query_Date", "Condition", "BriefTitle", "Acronym",
                               "PrimaryCompletionDate", "ResultsFirstSubmitDate")
  )

ISRCTN_Changes_COVID <-
  pull_change(con = con,
              registry_table = "ISRCTN",
              regex_pattern = prog_regexes[1],
              start_date = get_last_registry_entry_before_today("ISRCTN"),
              end_date = Sys.Date(),
              group_cols = c("Guideline.number", "ISRCTN_No"),
              exclude_cols = c("Query_Date", "Public_Title", "Acronym", "Scientific_Title", "URL")
  )

ISRCTN_Changes_IP <-
  pull_change(con = con,
              registry_table = "ISRCTN",
              regex_pattern = prog_regexes[2],
              start_date = get_last_registry_entry_before_today("ISRCTN"),
              end_date = Sys.Date(),
              group_cols = c("Guideline.number", "ISRCTN_No"),
              exclude_cols = c("Query_Date", "Public_Title", "Acronym", "Scientific_Title", "URL")
  )

ISRCTN_Changes_Surv_Oth <-
  pull_change(con = con,
              registry_table = "ISRCTN",
              regex_pattern = prog_regexes[3],
              start_date = get_last_registry_entry_before_today("ISRCTN"),
              end_date = Sys.Date(),
              group_cols = c("Guideline.number", "ISRCTN_No"),
              exclude_cols = c("Query_Date", "Public_Title", "Acronym", "Scientific_Title", "URL")
  )


#Pull NIHR info
pull_nihr <- function(prog_regex, old_or_new){
  
  dbReadTable(con, "NIHR") %>% filter(Query_Date == if (old_or_new == "old"){
                                                        get_last_registry_entry_before_today("NIHR")
                                                        } else if (old_or_new == "new") {
                                                        Sys.Date()  
                                                        },
                                      str_detect(Guideline.number, prog_regex))
  
}

nihr_old_COVID <- pull_nihr(prog_regex = prog_regexes[1], "old")
nihr_new_COVID <- pull_nihr(prog_regex = prog_regexes[1], "new")

nihr_old_IP <- pull_nihr(prog_regex = prog_regexes[2], "old")
nihr_new_IP <- pull_nihr(prog_regex = prog_regexes[2], "new")

nihr_old_Surv_Oth <- pull_nihr(prog_regex = prog_regexes[3], "old")
nihr_new_Surv_Oth <- pull_nihr(prog_regex = prog_regexes[3], "new")

NIHR_Changes_COVID <- compare_df(nihr_new_COVID,
                                 nihr_old_COVID,
                                 group_col = c("Guideline.number", "project_id"),
                                 exclude = c("Query_Date", "URL", "project_title"),
                                 stop_on_error = FALSE)

NIHR_Changes_IP <- compare_df(nihr_new_IP,
                              nihr_old_IP,
                              group_col = c("Guideline.number", "project_id"),
                              exclude = c("Query_Date", "URL", "project_title"),
                              stop_on_error = FALSE)

NIHR_Changes_Surv_Oth <- compare_df(nihr_new_Surv_Oth,
                                    nihr_old_Surv_Oth,
                                    group_col = c("Guideline.number", "project_id"),
                                    exclude = c("Query_Date", "URL", "project_title"),
                                    stop_on_error = FALSE)

EU_Changes_COVID <-
  pull_change(con = con,
              registry_table = "EU",
              regex_pattern = prog_regexes[1],
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

EU_Changes_IP <-
  pull_change(con = con,
              registry_table = "EU",
              regex_pattern = prog_regexes[2],
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

EU_Changes_Surv_Oth <-
  pull_change(con = con,
              registry_table = "EU",
              regex_pattern = prog_regexes[3],
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
  NCT_PM_COVID <- gen_pubmed_df(con, "NCT_PM", get_last_registry_entry_before_today("NCT_PM")+1, Sys.Date(), regex_pattern = prog_regexes[1])
  NCT_PM_IP <- gen_pubmed_df(con, "NCT_PM", get_last_registry_entry_before_today("NCT_PM")+1, Sys.Date(), regex_pattern = prog_regexes[2])
  NCT_PM_Surv_Oth <- gen_pubmed_df(con, "NCT_PM", get_last_registry_entry_before_today("NCT_PM")+1, Sys.Date(), regex_pattern = prog_regexes[3])
}
if ("ISRCTN_PM" %in% dbListTables(con)) {
  ISRCTN_PM_COVID <- gen_pubmed_df(con, "ISRCTN_PM", get_last_registry_entry_before_today("ISRCTN_PM")+1, Sys.Date(), regex_pattern = prog_regexes[1])
  ISRCTN_PM_IP <- gen_pubmed_df(con, "ISRCTN_PM", get_last_registry_entry_before_today("ISRCTN_PM")+1, Sys.Date(), regex_pattern = prog_regexes[2])
  ISRCTN_PM_Surv_Oth <- gen_pubmed_df(con, "ISRCTN_PM", get_last_registry_entry_before_today("ISRCTN_PM")+1, Sys.Date(),  regex_pattern = prog_regexes[3])
}
if ("NIHR_PM" %in% dbListTables(con)) {
  NIHR_PM_COVID <- gen_pubmed_df(con, "NIHR_PM", get_last_registry_entry_before_today("NIHR_PM") + 1, Sys.Date(), regex_pattern = prog_regexes[1])
  NIHR_PM_IP <- gen_pubmed_df(con, "NIHR_PM", get_last_registry_entry_before_today("NIHR_PM") + 1, Sys.Date(), regex_pattern = prog_regexes[2])
  NIHR_PM_Surv_Oth <- gen_pubmed_df(con, "NIHR_PM", get_last_registry_entry_before_today("NIHR_PM") + 1, Sys.Date(), regex_pattern = prog_regexes[3])
}
if ("EU_PM" %in% dbListTables(con)) {
  EU_PM_COVID <- gen_pubmed_df(con, "EU_PM", get_last_registry_entry_before_today("EU_PM") + 1, Sys.Date(), regex_pattern = prog_regexes[1])
  EU_PM_IP <- gen_pubmed_df(con, "EU_PM", get_last_registry_entry_before_today("EU_PM") + 1, Sys.Date(), regex_pattern = prog_regexes[2])
  EU_PM_Surv_Oth <- gen_pubmed_df(con, "EU_PM", get_last_registry_entry_before_today("EU_PM") + 1, Sys.Date(), regex_pattern = prog_regexes[3])
}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                #create attachments
write_changes_to_disk(NCT_Changes_COVID, "NCT_COVID")
write_changes_to_disk(NCT_Changes_IP, "NCT_IP")
write_changes_to_disk(NCT_Changes_Surv_Oth, "NCT_OTH")
write_changes_to_disk(ISRCTN_Changes_COVID, "ISRCTN_IP")
write_changes_to_disk(ISRCTN_Changes_IP, "ISRCTN_IP")
write_changes_to_disk(ISRCTN_Changes_Surv_Oth, "ISRCTN_IP")
write_changes_to_disk(NIHR_Changes_COVID, "NIHR_COVID")
write_changes_to_disk(NIHR_Changes_IP, "NIHR_IP")
write_changes_to_disk(NIHR_Changes_Surv_Oth, "NIHR_OTH")
write_changes_to_disk(EU_Changes_COVID, "EU_COVID")
write_changes_to_disk(EU_Changes_IP, "EU_IP")
write_changes_to_disk(EU_Changes_Surv_Oth, "EU_OTH")

write_PM_dfs_to_disk(NCT_PM_COVID, "NCT_COVID")
write_PM_dfs_to_disk(NCT_PM_IP, "NCT_IP")
write_PM_dfs_to_disk(NCT_PM_Surv_Oth, "NCT_OTH")
write_PM_dfs_to_disk(ISRCTN_PM_COVID, "ISRCTN_COVID")
write_PM_dfs_to_disk(ISRCTN_PM_IP, "ISRCTN_IP")
write_PM_dfs_to_disk(ISRCTN_PM_Surv_Oth, "ISRCTN_OTH")
write_PM_dfs_to_disk(NIHR_PM_COVID, "NIHR_COVID")
write_PM_dfs_to_disk(NIHR_PM_IP, "NIHR_IP")
write_PM_dfs_to_disk(NIHR_PM_Surv_Oth, "NIHR_OTH")
write_PM_dfs_to_disk(EU_PM_COVID, "EU_COVID")
write_PM_dfs_to_disk(EU_PM_IP, "EU_IP")
write_PM_dfs_to_disk(EU_PM_Surv_Oth, "EU_OTH")

# Close con
dbDisconnect(con)

#Create files lists
COVID_files <- dir(daily_path, full.names = TRUE) %>% str_subset("_COVID_")
IP_files <- dir(daily_path, full.names = TRUE) %>% str_subset("_IP_")
OTH_files <- dir(daily_path, full.names = TRUE) %>% str_subset("_OTH_")

# create emails, attach files and send

#COVID
COVID_email <- envelope() %>%
  from("robert.willans@nice.org.uk") %>%
  to("niamh.knapton@nice.org.uk") %>%
  cc(
  "catherine.jacob@nice.org.uk",
	"robert.willans@nice.org.uk") %>%
  text(
    paste("Trial Tracking Changes - COVID",
          Sys.Date(),
          "Note that this email inbox is not monitored - please email robert.willans@nice.org.uk with any queries",
          sep = " - ")
  ) %>%
  subject(subject = "Trial Tracking Changes - COVID")

if (length(COVID_files) > 0) {
  for (i in 1:length(COVID_files)) {
    COVID_email <- COVID_email %>% 
      attachment(COVID_files[i])
  }  
}

if (length(COVID_email$parts) > 1) {
  smtp <- server(host = "smtp.mandrillapp.com",
                 port = 587,
                 username = "nice",
                 password = read_file(here("Data_Files", "mandrill_pwd.txt")))
  
  smtp(COVID_email, verbose = FALSE)
  
} else {
  COVID_email <- envelope() %>% 
    from("robert.willans@nice.org.uk") %>% 
    to("robert.willans@nice.org.uk") %>% 
    subject("TrialTracker ran today - COVID") %>% 
    text(paste0("TrialTracker script completed successfully today (", Sys.Date(), ") but noted no changes"))
  
  smtp <- server(host = "smtp.mandrillapp.com",
                 port = 587,
                 username = "nice",
                 password = read_file(here("Data_Files", "mandrill_pwd.txt")))
  
  smtp(COVID_email, verbose = FALSE)
}

#IP
IP_email <- envelope() %>%
  from("robert.willans@nice.org.uk") %>%
  to("niamh.knapton@nice.org.uk") %>%
  cc(
    "catherine.jacob@nice.org.uk",
     "robert.willans@nice.org.uk") %>%
  text(
    paste("Trial Tracking Changes - IP",
          Sys.Date(),
          "Note that this email inbox is not monitored - please email robert.willans@nice.org.uk with any queries",
          sep = " - ")
  ) %>%
  subject(subject = "Trial Tracking Changes - IP")

if (length(IP_files) > 0) {
  for (i in 1:length(IP_files)) {
    IP_email <- IP_email %>% 
      attachment(IP_files[i])
  }  
}

if (length(IP_email$parts) > 1) {
  smtp <- server(host = "smtp.mandrillapp.com",
                 port = 587,
                 username = "nice",
                 password = read_file(here("Data_Files", "mandrill_pwd.txt")))
  
  smtp(IP_email, verbose = FALSE)
  
} else {
  IP_email <- envelope() %>% 
    from("robert.willans@nice.org.uk") %>% 
    to("robert.willans@nice.org.uk") %>% 
    subject("TrialTracker ran today - IP") %>% 
    text(paste0("TrialTracker script completed successfully today (", Sys.Date(), ") but noted no changes"))
  
  smtp <- server(host = "smtp.mandrillapp.com",
                 port = 587,
                 username = "nice",
                 password = read_file(here("Data_Files", "mandrill_pwd.txt")))
  
  smtp(IP_email, verbose = FALSE)
}

#OTHER
OTH_email <- envelope() %>%
  from("robert.willans@nice.org.uk") %>%
  to("niamh.knapton@nice.org.uk") %>%
  cc(
    "catherine.jacob@nice.org.uk",
    "robert.willans@nice.org.uk") %>%
  text(
    paste("Trial Tracking Changes - Other",
          Sys.Date(),
          "Note that this email inbox is not monitored - please email robert.willans@nice.org.uk with any queries",
          sep = " - ")
  ) %>%
  subject(subject = "Trial Tracking Changes - Other")

if (length(OTH_files) > 0) {
  for (i in 1:length(OTH_files)) {
    OTH_email <- OTH_email %>% 
      attachment(OTH_files[i])
  }  
}

if (length(OTH_email$parts) > 1) {
  smtp <- server(host = "smtp.mandrillapp.com",
                 port = 587,
                 username = "nice",
                 password = read_file(here("Data_Files", "mandrill_pwd.txt")))
  
  smtp(OTH_email, verbose = FALSE)
  
} else {
  OTH_email <- envelope() %>% 
    from("robert.willans@nice.org.uk") %>% 
    to("robert.willans@nice.org.uk") %>% 
    subject("TrialTracker ran today - Other") %>% 
    text(paste0("TrialTracker script completed successfully today (", Sys.Date(), ") but noted no changes"))
  
  smtp <- server(host = "smtp.mandrillapp.com",
                 port = 587,
                 username = "nice",
                 password = read_file(here("Data_Files", "mandrill_pwd.txt")))
  
  smtp(OTH_email, verbose = FALSE)
}