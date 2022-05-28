# Downloads data for trials (schedule to daily)
# Save to SQLite db

#options echo true
options(echo = TRUE)

#setwd - ugly hack probably need rstudio server on here
setwd('//srv/shiny-server/trialtracker')

# libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(xml2)
library(DBI)
library(ctrdata)
library(easyPubMed)
library(rentrez)
library(here)

#print date for debug purposes
Sys.Date()

#set permissions for clinicaltrials.eu
httr::set_config(httr::config(ssl_verifypeer = FALSE))

## FUNCTIONS

# collapse ID
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
results_count <- function(results_obj, index) {
  results_obj[[index]]$count
}
get_xml_for_results <- function(results_object_item, count_list_item) {
  if (count_list_item == 0) {
    return(NULL)
  }
  else {
    return(
      articles_to_list(
        entrez_fetch(db = 'pubmed', results_object_item$ids, rettype = 'xml')
      ))
  }
}
format_results_item <- function(results_item) {
  map(results_item, article_to_df, getAuthor = FALSE, max_chars = -1) %>%
    bind_rows()
}
get_dfs_from_results_obj <- function(results_obj_item, count_list_item) {
  if (count_list_item == 0) {
    return(NULL)
  }
  else {
    return(
      format_results_item(results_obj_item)
    )
  }
}
sleepy_entrez_search <- function(search_term, db = 'pubmed'){
  Sys.sleep(0.3)
  return(entrez_search(db = db, term = search_term))
}
create_search_list <- function(id_vector){
  
  tibble(ids = id_vector) %>% 
    filter(!is.na(ids)) %>%
    distinct() %>% 
    mutate(ids = paste0(ids, " AND ",format(Sys.Date() - 1, "%Y/%m/%d"),"[edat]" )) %>% 
    as_vector() %>% 
    unname()
}
get_pubmed_arts <- function(ID_list) {
  Results1 <- map(ID_list, sleepy_entrez_search, db = 'pubmed')
  Counts <- map(seq_along(Results1), results_count, results_obj = Results1)
  Results2 <- map2(Results1, Counts, get_xml_for_results)
  Results3 <- map2(Results2, Counts, get_dfs_from_results_obj)
  return(Results3)
}
format_arts_list <- function(arts_list) {
  df1 <- arts_list %>%
    bind_rows()
  if ("pmid" %in% colnames(df1)) {
    return(
      df1 %>%
        filter(!is.na(pmid)) %>%
        rename("search" = ...15) %>%
        mutate(
          ID = str_extract(search, "([^\\s]+)"),
          Query_Date = Sys.Date(),
          doi = case_when((doi != "") ~ paste0('https://dx.doi.org/', doi),
                          TRUE ~ "")
        ) %>%
        select(Query_Date, ID, doi, search, pmid:journal)
    )
  } else
    return(NULL)
}
create_arts_DF <- function(search_query_list) {
  get_pubmed_arts(search_query_list) %>% 
    map2(search_query_list, bind_cols) %>% 
    format_arts_list()
}

# setup con to db
con <- dbConnect(RSQLite::SQLite(), here("RSQLite_Data", "TrialTracker-db.sqlite"))

# Generate ID vectors
Trial_IDs <- dbReadTable(con, "Trial_IDs")
half_ISRCTN_Id_Vector <- round(length(concat_ids(Trial_IDs, "ISRCTN_Ids"))/2,0)

# Collapse NCT numbers into search term
NCT_Id_Vector <- collapse_ids(Trial_IDs, "NCT_Ids", "%20OR%20")
#ISRCTN_Id_Vector <- collapse_ids(Trial_IDs, "ISRCTN_Ids", "%20OR%20")
ISRCTN_Id_Vector1 <- concat_ids(Trial_IDs, "ISRCTN_Ids")[1:half_ISRCTN_Id_Vector] %>% paste0(collapse = "%20OR%20")
ISRCTN_Id_Vector2 <- concat_ids(Trial_IDs, "ISRCTN_Ids")[(half_ISRCTN_Id_Vector + 1):length(concat_ids(Trial_IDs, "ISRCTN_Ids"))] %>% paste0(collapse = "%20OR%20")
NIHR_Id_Vector <- collapse_ids(Trial_IDs, "NIHR_Ids", "+OR+")
EU_Vector <- collapse_ids(Trial_IDs, "EU_Ids", "+OR+")

# Construct URLs
# NCT
NCT_URL <- paste0(
  "https://www.clinicalTrials.gov/api/query/study_fields?expr=",
  NCT_Id_Vector,
  "&fields=",
  paste("NCTid", "OrgStudyId", "Condition", "BriefTitle", "Acronym",
        "OverallStatus", "PrimaryCompletionDate",
        "CompletionDate", "ResultsFirstSubmitDate", "ResultsFirstPostDate",
        "LastUpdatePostDate", "SeeAlsoLinkURL",
        sep = ","
  ),
  "&min_rnk=1",
  "&max_rnk=",
  sum(!is.na(Trial_IDs$NCT_Ids)) + 10,
  "&fmt=csv"
)

# ISRCTN_URL <- paste0(
#   "http://www.isrctn.com/api/query/format/who?q=",
#   ISRCTN_Id_Vector,
#   "&limit=",
#   length(concat_ids(Trial_IDs, "ISRCTN_Ids")) + 10
# )

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

NIHR_URL <- paste0(
  "https://nihr.opendatasoft.com/api/records/1.0/search/?dataset=infonihr-open-dataset&q=",
  NIHR_Id_Vector,
  "&rows=",
  length(concat_ids(Trial_IDs, "NIHR_Ids")) + 50
)

EU_URL <- paste0(
  "https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
  EU_Vector
)

rm(EU_Vector, ISRCTN_Id_Vector1, ISRCTN_Id_Vector2, NCT_Id_Vector, NIHR_Id_Vector)

# Results DFs

# NCT
#verbose = true because for some reason this fails on linux otherwise....
NCT_DF <- GET(NCT_URL, verbose = TRUE) %>%
  content() %>%
  read_csv(skip = 9) %>%
  right_join(Trial_IDs[, c("Guideline.number", "URL", "NCT_Ids")], by = c("NCTId" = "NCT_Ids")) %>%
  filter(!is.na(NCTId)) %>%
  mutate(Query_Date = Sys.Date()) %>%
  select(Query_Date, Guideline.number, URL, everything(), -Rank)

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
  right_join(Trial_IDs[, c("Guideline.number", "ISRCTN_Ids")], by = c("ISRCTN_No" = "ISRCTN_Ids")) %>%
  filter(!is.na(ISRCTN_No)) %>%
  mutate(Query_Date = Sys.Date()) %>%
  select(Query_Date, Guideline.number, URL, everything())

# NIHR
NIHR_json <- fromJSON(url(NIHR_URL))

NIHR_Trial_IDs <- Trial_IDs %>%
  select(Guideline.number, URL, NIHR_Ids) %>%
  filter(!is.na(NIHR_Ids)) %>%
  mutate("NIHR_Ids" = str_replace_all(NIHR_Ids, "[^\\d]", ""))

NIHR_DF <- NIHR_json$records$fields %>%
  mutate("projectjoin" = str_replace_all(project_id, "[^\\d]", "")) %>%
  right_join(NIHR_Trial_IDs, by = c("projectjoin" = "NIHR_Ids")) %>%
  filter(!is.na(projectjoin)) %>%
  mutate(Query_Date = Sys.Date()) %>%
  select(Query_Date, Guideline.number, URL, project_id, project_title, project_status, project_id, end_date)

rm(NIHR_Trial_IDs, NIHR_json)

# EU

# sqlite db
eu_temp_db <- nodbi::src_sqlite(dbname = here("RSQLite_Data", "EU_temp_db.sqlite"), collection = "EU")

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
  right_join(Trial_IDs) %>%
  filter(!is.na(`_id`)) %>%
  mutate(Query_Date = Sys.Date()) %>%
  select(Query_Date, Guideline.number, URL, everything(),
         -NCT_Ids, -ISRCTN_Ids, -NIHR_Ids,
         -Short..working.title.) %>% 
  rename('X_id' = `_id`) %>% 
  unique()

# Add to db if no record already today
update_db(con, "NCT", NCT_DF)
update_db(con, "ISRCTN", ISRCTN_DF)
update_db(con, "NIHR", NIHR_DF)
update_db(con, "EU", EU_DF)

rm(NCT_DF, ISRCTN_DF, NIHR_DF, EU_DF, NCT_URL, ISRCTN_URL1, ISRCTN_URL2, NIHR_URL, EU_URL)