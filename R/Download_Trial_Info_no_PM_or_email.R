# Downloads data for trials (schedule to daily)
# Save to SQLite db

#options echo true
options(echo = TRUE)

# Need to add a config or similar package to set path (or manually do this)
#setwd("/srv/shiny-server/trialtracker/")
setwd("C:/RStudio_Projects/trialtracker/")

#libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(xml2)
library(DBI)
library(ctrdata)

#print date for debug purposes
Sys.Date()

#set permissions for clinicaltrials.eu to work
httr::set_config(httr::config(ssl_verifypeer = FALSE))

## FUNCTIONS
source('R/Functions/download_trial_info_functions.R') # download functions
source('R/Functions/PM_API_functions.R') # pubmed API functions

# setup con to db
con <- dbConnect(RSQLite::SQLite(), "RSQLite_Data/TrialTracker-db.sqlite")

# Generate ID vectors
Trial_IDs <- dbReadTable(con, "Trial_IDs")
half_ISRCTN_Id_Vector <- round(length(concat_ids(Trial_IDs, "ISRCTN_Ids"))/2,0)
half_NCT_Id_Vector <- round(length(concat_ids(Trial_IDs, "NCT_Ids"))/2,0)

# Collapse Trial ID numbers into search term
#NCT_Id_Vector <- collapse_ids(Trial_IDs, "NCT_Ids", "%7C")
NCT_Id_Vector1 <- concat_ids(Trial_IDs, "NCT_Ids")[1:half_NCT_Id_Vector] |> paste0(collapse = "%7C")
NCT_Id_Vector2 <- concat_ids(Trial_IDs, "NCT_Ids")[(half_NCT_Id_Vector + 1):length(concat_ids(Trial_IDs, "NCT_Ids"))] |> paste0(collapse = "%7C")
ISRCTN_Id_Vector1 <- concat_ids(Trial_IDs, "ISRCTN_Ids")[1:half_ISRCTN_Id_Vector] %>% paste0(collapse = "%20OR%20")
ISRCTN_Id_Vector2 <- concat_ids(Trial_IDs, "ISRCTN_Ids")[(half_ISRCTN_Id_Vector + 1):length(concat_ids(Trial_IDs, "ISRCTN_Ids"))] %>% paste0(collapse = "%20OR%20")
NIHR_Id_Vector <- collapse_ids(Trial_IDs, "NIHR_Ids", "+OR+")
EU_Vector <- collapse_ids(Trial_IDs, "EU_Ids", "+OR+")

# Construct URLs

# The clinicalTrial.gov API can handle 1000 request at a time. Above 1000 and the extract will need to be split.
# Split URL into 2 as URL too long now

NCT_URL1 <- generate_NCT_URL(NCT_Id_Vector1)
NCT_URL2 <- generate_NCT_URL(NCT_Id_Vector2)

#ISRCTN URL done in two parts as v large

ISRCTN_URL1 <- paste0(
  "http://www.isrctn.com/api/query/format/who?q=",
  ISRCTN_Id_Vector1,
  "&limit=",
  length(concat_ids(Trial_IDs, "ISRCTN_Ids")) + 10
)

ISRCTN_URL2 <- paste0(
  "http://www.isrctn.com/api/query/format/who?q=",
  ISRCTN_Id_Vector2,
  "&limit=",
  length(concat_ids(Trial_IDs, "ISRCTN_Ids")) + 10
)

NIHR_URL_API2 <-
  "https://nihr.opendatasoft.com/api/v2/catalog/datasets/infonihr-open-dataset/exports/json?limit=-1&offset=0&lang=en&timezone=UTC"

EU_URL <- paste0(
  "https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
  EU_Vector
)

rm(EU_Vector, NCT_Id_Vector1, NCT_Id_Vector2, NIHR_Id_Vector, ISRCTN_Id_Vector1, ISRCTN_Id_Vector2)

# Results DFs

# NCT
NCT_DF1 <- generate_NCT_DF(NCT_URL1)
NCT_DF2 <- generate_NCT_DF(NCT_URL2)
NCT_DF <- bind_rows(NCT_DF1, NCT_DF2)

NCT_DF <- NCT_DF %>% 
  right_join(Trial_IDs[, c("Program", "Guideline.number", "URL", "NCT_Ids")], 
             by = c("NCTId" = "NCT_Ids"), multiple = "all") %>%
  filter(!is.na(NCTId)) %>%
  mutate(Query_Date = Sys.Date()) %>%
  select(Query_Date, Program, Guideline.number, URL, everything(), -Rank)

# ISRCTN
ISRCTN_DF1 <- generate_ISRCTN_df(ISRCTN_URL1)
ISRCTN_DF2 <- generate_ISRCTN_df(ISRCTN_URL2)

ISRCTN_DF <- bind_rows(ISRCTN_DF1, ISRCTN_DF2) %>%
  right_join(Trial_IDs[, c("Program", "Guideline.number", "ISRCTN_Ids")], by = c("ISRCTN_No" = "ISRCTN_Ids"), multiple = "all") %>%
  filter(!is.na(ISRCTN_No)) %>%
  mutate(Query_Date = Sys.Date()) %>%
  select(Query_Date, Program, Guideline.number, URL, everything())

# NIHR
NIHR_json <- fromJSON(url(NIHR_URL_API2))

NIHR_Trial_IDs <- Trial_IDs %>%
  select(Program, Guideline.number, URL, NIHR_Ids) %>%
  drop_na(NIHR_Ids) %>%
  mutate("projectjoin" = str_replace_all(NIHR_Ids, "[^\\d]", ""))

NIHR_DF <- NIHR_json %>%
  mutate("projectjoin" = str_replace_all(project_id, "[^\\d]", "")) %>%
  right_join(NIHR_Trial_IDs, by = c("projectjoin"), multiple = "all") %>%
  drop_na(projectjoin) %>%
  mutate(Query_Date = Sys.Date()) %>%
  select(Query_Date, Program, Guideline.number, URL, project_id, project_title, project_status, project_id, end_date)

rm(NIHR_Trial_IDs, NIHR_json)

# EU

# sqlite db
eu_temp_db <- nodbi::src_sqlite(dbname = "RSQLite_Data/EU_temp_db.sqlite", collection = "EU")

try(ctrLoadQueryIntoDb(queryterm = EU_URL, con = eu_temp_db))

# Collapse fields into 1 vector
EU_DF <-
  dbGetFieldsIntoDf(
    str_subset(
      c(dbFindFields("end_of_trial", con = eu_temp_db),
        dbFindFields("a3", con = eu_temp_db),
        dbFindFields("a4", con = eu_temp_db)
      ),
      "_it|_es|_fr|_nl",
      negate = TRUE),
    con = eu_temp_db
  ) %>%
  mutate(EU_Ids = str_extract(`_id`, "^\\d{4}-\\d{6}-\\d{2}")) %>%
  right_join(Trial_IDs, multiple = "all") %>%
  filter(!is.na(`_id`)) %>%
  mutate(Query_Date = Sys.Date()) %>%
  select(Query_Date, Program, Guideline.number, URL, everything(),
         -NCT_Ids, -ISRCTN_Ids, -NIHR_Ids,
         -Short..working.title.) %>%
  rename('X_id' = `_id`) %>%
  unique()

# Add to db if no record already today
update_db(con, "NCT", NCT_DF)
update_db(con, "ISRCTN", ISRCTN_DF)
update_db(con, "NIHR", NIHR_DF)
update_db(con, "EU", EU_DF)

rm(NCT_DF, ISRCTN_DF, NIHR_DF, EU_DF, NCT_URL1, NCT_URL2, ISRCTN_URL1, ISRCTN_URL2, NIHR_URL_API2, EU_URL)