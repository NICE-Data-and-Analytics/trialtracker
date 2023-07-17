# Downloads data for trials (schedule to daily)
# Save to SQLite db

#options echo true
options(echo = TRUE)

#setpath
path <- "C:/RStudio_Projects/trialtracker/"
#path <- "/srv/shiny-server/trialtracker-dev/"

# libraries
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

#Concatenation functions used for constructing API urls
concat_ids <- function(df, id_col) {
  
  df <- df %>%
    select(all_of(id_col)) %>%
    drop_na()
  
  if (nrow(df) > 0) {
      
    return(
      df %>%
        filter(if_any(.cols = everything(), .fns = ~(. !=""))) %>% 
        unique() %>%
        as_vector() %>%
        unname()
  )
  }
  
  else
  
    return(
      df %>% 
        unique() %>%
        as_vector() %>%
        unname()
           )  
    
}
collapse_ids <- function(df, id_col, paste_collapse) {
  concat_ids(df, id_col) %>%
    paste0(collapse = paste_collapse)
}

#Create a list of ids to search
create_search_list <- function(id_vector){
  
  tibble(ids = id_vector) %>%
    filter(!is.na(ids)) %>%
    distinct() %>%
    as_vector() %>%
    unname()
}

#Updates the db with info
update_db <- function(con, registry, DF) {
  
  if (!is.null(DF)) {
    
    if (!(registry %in% dbListTables(con))) {
      dbCreateTable(con, registry, DF)
      dbAppendTable(con, registry, DF)
    }
    
    else if (nrow(dbReadTable(con, registry) %>% filter(Query_Date == Sys.Date())) == 0) {
      dbAppendTable(con, registry, DF)
    }
    
    else if (nrow(dbReadTable(con, registry) %>% filter(Query_Date == Sys.Date())) > 0) {
      temp_df <- dbReadTable(con, registry) %>% filter(Query_Date < Sys.Date())
      dbWriteTable(con, registry, temp_df, overwrite = TRUE)
      dbAppendTable(con, registry, DF)
    }
    
  }
  
}

#source pubmed API functions
source(paste0(path, 'PM_API_functions.R'))

# setup con to db
con <- dbConnect(RSQLite::SQLite(), paste0(path, "RSQLite_Data/TrialTracker-db.sqlite"))

# Generate ID vectors
Trial_IDs <- dbReadTable(con, "Trial_IDs")
half_ISRCTN_Id_Vector <- round(length(concat_ids(Trial_IDs, "ISRCTN_Ids"))/2,0)
half_NCT_Id_Vector <- round(length(concat_ids(Trial_IDs, "NCT_Ids"))/2,0)

# Collapse Trial ID numbers into search term
#NCT_Id_Vector <- collapse_ids(Trial_IDs, "NCT_Ids", "%20OR%20")
NCT_Id_Vector1 <- concat_ids(Trial_IDs, "NCT_Ids")[1:half_NCT_Id_Vector] %>% paste0(collapse = "%20OR%20")
NCT_Id_Vector2 <- concat_ids(Trial_IDs, "NCT_Ids")[(half_NCT_Id_Vector + 1):length(concat_ids(Trial_IDs, "NCT_Ids"))] %>% paste0(collapse = "%20OR%20")
ISRCTN_Id_Vector1 <- concat_ids(Trial_IDs, "ISRCTN_Ids")[1:half_ISRCTN_Id_Vector] %>% paste0(collapse = "%20OR%20")
ISRCTN_Id_Vector2 <- concat_ids(Trial_IDs, "ISRCTN_Ids")[(half_ISRCTN_Id_Vector + 1):length(concat_ids(Trial_IDs, "ISRCTN_Ids"))] %>% paste0(collapse = "%20OR%20")
NIHR_Id_Vector <- collapse_ids(Trial_IDs, "NIHR_Ids", "+OR+")
EU_Vector <- collapse_ids(Trial_IDs, "EU_Ids", "+OR+")

# Construct URLs
# NCT URL done in two parts as v large

generate_NCT_URL <- function(NCT_ID_Vec, half_vec_length){
  
  paste0(
    "https://www.clinicalTrials.gov/api/query/study_fields?expr=",
    NCT_ID_Vec,
    "&fields=",
    paste("NCTid", "OrgStudyId", "Condition", "BriefTitle", "Acronym",
          "OverallStatus", "PrimaryCompletionDate",
          "CompletionDate", "ResultsFirstSubmitDate", "ResultsFirstPostDate",
          "LastUpdatePostDate", "SeeAlsoLinkURL",
          sep = ","
    ),
    "&min_rnk=1",
    "&max_rnk=",
    (half_vec_length + 10),
    "&fmt=csv"
  )
  
}

NCT_URL_1 <- generate_NCT_URL(NCT_Id_Vector1, half_NCT_Id_Vector)
NCT_URL_2 <- generate_NCT_URL(NCT_Id_Vector2, half_NCT_Id_Vector)


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
#verbose = true because for some reason this fails on linux otherwise....
generate_NCT_DF <- function(url){
  
  GET(url, verbose = TRUE) %>%
    content() %>%
    read_csv(skip = 9) 
  
}

NCT_DF_1 <- generate_NCT_DF(NCT_URL_1)
NCT_DF_2 <- generate_NCT_DF(NCT_URL_2)

NCT_DF <- bind_rows(NCT_DF_1, NCT_DF_2) |> 
  right_join(Trial_IDs[, c("Program", "Guideline.number", "URL", "NCT_Ids")], by = c("NCTId" = "NCT_Ids"), multiple = "all") %>%
  filter(!is.na(NCTId)) %>%
  mutate(Query_Date = Sys.Date()) %>%
  select(Query_Date, Program, Guideline.number, URL, everything(), -Rank)

# ISRCTN
generate_ISRCTN_df <- function(ISRCTN_URL){
  
  ISRCTN_XML <- GET(ISRCTN_URL) %>% content()
  
  tibble(
    "ISRCTN_No" = xml_text(xml_find_all(ISRCTN_XML, ".//trial_id")),
    "Public_Title" = xml_text(xml_find_all(ISRCTN_XML, ".//public_title")),
    "Acronym" = xml_text(xml_find_all(ISRCTN_XML, ".//acronym")),
    "Scientific_Title" = xml_text(xml_find_all(ISRCTN_XML, ".//scientific_title")),
    "URL" = xml_text(xml_find_all(ISRCTN_XML, ".//url")),
    "Recruitment_Status" = xml_text(xml_find_all(ISRCTN_XML, ".//recruitment_status")),
    "Results_date_completed" = xml_text(xml_find_all(ISRCTN_XML, ".//results_date_completed")),
    "Results_url_link" = xml_text(xml_find_all(ISRCTN_XML, ".//results_url_link")),
    "Results_summary" = xml_text(xml_find_all(ISRCTN_XML, ".//results_summary")),
    "Results_date_posted" = xml_text(xml_find_all(ISRCTN_XML, ".//results_date_posted")),
    "Results_date_first_publication" =
      xml_text(
        xml_find_all(ISRCTN_XML, ".//results_date_first_publication")
      )
  )
}

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
eu_temp_db <- nodbi::src_sqlite(dbname = paste0(path, "RSQLite_Data/EU_temp_db.sqlite"), collection = "EU")

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
    con = eu_temp_db, stopifnodata = FALSE
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

rm(NCT_DF, ISRCTN_DF, NIHR_DF, EU_DF, NCT_URL, ISRCTN_URL1, ISRCTN_URL2, NIHR_URL_API2, EU_URL)

# pubmed call

# set entrez key
api <- read_file(paste0(path, 'Data_Files/entrez.key'))

# Generate search lists
NCT_PM_Searches <- create_search_list(Trial_IDs$NCT_Ids)
ISRCTN_PM_Searches <- create_search_list(Trial_IDs$ISRCTN_Ids)
NIHR_PM_Searches <- create_search_list(Trial_IDs$NIHR_Ids)
EU_PM_Searches <- create_search_list(Trial_IDs$EU_Ids)

NCT_PM_DF <- generate_pm_tibble_from_search_term_series(NCT_PM_Searches, api_object = api, mindate = Sys.Date()-1, maxdate = Sys.Date()-1) %>%
  {if (nrow(.)==0) . else left_join(.,Trial_IDs, by = c("ID" = "NCT_Ids"))} %>% 
  {if (nrow(.)==0) . else select(., Program, Guideline.number, everything(), -ISRCTN_Ids, -NIHR_Ids, -EU_Ids)} %>% 
  distinct()

ISRCTN_PM_DF <- generate_pm_tibble_from_search_term_series(ISRCTN_PM_Searches, api_object = api, mindate = Sys.Date()-1, maxdate = Sys.Date()-1) %>%
  {if (nrow(.)==0) . else left_join(.,Trial_IDs, by = c("ID" = "ISRCTN_Ids"))} %>% 
  {if (nrow(.)==0) . else select(., Program, Guideline.number, everything(), -NCT_Ids, -NIHR_Ids, -EU_Ids)} %>% 
  distinct()

NIHR_PM_DF <- generate_pm_tibble_from_search_term_series(NIHR_PM_Searches, api_object = api, mindate = Sys.Date()-1, maxdate = Sys.Date()-1) %>%
  {if (nrow(.)==0) . else left_join(.,Trial_IDs, by = c("ID" = "NIHR_Ids"))} %>% 
  {if (nrow(.)==0) . else select(., Program, Guideline.number, everything(), -ISRCTN_Ids, -NCT_Ids, -EU_Ids)} %>% 
  distinct()

EU_PM_DF <- generate_pm_tibble_from_search_term_series(EU_PM_Searches, api_object = api, mindate = Sys.Date()-1, maxdate = Sys.Date()-1) %>%
  {if (nrow(.)==0) . else left_join(.,Trial_IDs, by = c("ID" = "EU_Ids"))} %>% 
  {if (nrow(.)==0) . else select(., Program, Guideline.number, everything(), -ISRCTN_Ids, -NIHR_Ids, -NCT_Ids)}  %>% 
  distinct()

if(nrow(NCT_PM_DF)>0){update_db(con, "NCT_PM", NCT_PM_DF)}
if(nrow(ISRCTN_PM_DF)>0){update_db(con, "ISRCTN_PM", ISRCTN_PM_DF)}
if(nrow(NIHR_PM_DF)>0){update_db(con, "NIHR_PM", NIHR_PM_DF)}
if(nrow(EU_PM_DF)>0){update_db(con, "EU_PM", EU_PM_DF)}

rm(NCT_PM_Searches, ISRCTN_PM_Searches, NIHR_PM_Searches, EU_PM_Searches, Trial_IDs)

# disconnect db
dbDisconnect(con)

### EMAIL SECTION

source(paste0(path, 'Email_Alerts.R'), echo = TRUE)