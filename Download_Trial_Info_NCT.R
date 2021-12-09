# Downloads data for trials (schedule to daily)
# Save to SQLite db

# libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(xml2)
library(DBI)
library(ctrdata)
library(easyPubMed)
library(rentrez)

#ugly setwd hack
setwd('//srv/shiny-server/trialtracker')

## FUNCTIONS

# collapse ID
concat_ids <- function(df, id_col) {
  df %>%
    select(all_of(id_col)) %>%
    drop_na() %>%
    filter(across() != "") %>% 
    unique() %>%
    as_vector() %>%
    unname()
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
get_pubmed_arts <- function(ID_list) {
  Results1 <- map(ID_list, entrez_search, db = 'pubmed')
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
con <- dbConnect(RSQLite::SQLite(), "RSQLite_Data/TrialTracker-db.sqlite")

# Generate ID vectors
Trial_IDs <- dbReadTable(con, "Trial_IDs")

# Collapse NCT numbers into search term
NCT_Id_Vector <- collapse_ids(Trial_IDs, "NCT_Ids", "%20OR%20")

# Construct URLs
# NCT
NCT_URL <- paste0(
  "http://ClinicalTrials.gov/api/query/study_fields?expr=",
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

rm(NCT_Id_Vector)

# Results DFs

# NCT
NCT_DF <- GET(NCT_URL) %>%
  content() %>%
  read_csv(skip = 9) %>%
  right_join(Trial_IDs[, c("Guideline.number", "URL", "NCT_Ids")], by = c("NCTId" = "NCT_Ids")) %>%
  filter(!is.na(NCTId)) %>%
  mutate(Query_Date = Sys.Date()) %>%
  select(Query_Date, Guideline.number, URL, everything(), -Rank)

# Add to db 
update_db(con, "NCT", NCT_DF)

rm(NCT_DF, NCT_URL)

# # pubmed call - commented out
# 
# # set entrez key
# api <- read_file('Data_Files/entrez.key')
# set_entrez_key(api)
# 
# # Generate search lists
# NCT_PM_Searches <- Trial_IDs %>%
#   select(NCT_Ids) %>%
#   filter(!is.na(NCT_Ids)) %>%
#   distinct() %>%
#   mutate(NCT_Ids = paste0(NCT_Ids, " AND ", format(Sys.Date() - 1, "%Y/%m/%d"), "[edat]")) %>%
#   as_vector() %>%
#   unname()
# 
# NCT_PM_DF <- create_arts_DF(NCT_PM_Searches) %>%
#   {if (is.null(.)) . else left_join(.,Trial_IDs, by = c("ID" = "NCT_Ids"))} %>% 
#   {if (is.null(.)) . else select(.,Guideline.number, everything(), -ISRCTN_Ids, -NIHR_Ids, -EU_Ids)}  
#   
# update_db(con, "NCT_PM", NCT_PM_DF)
# 
# rm(NCT_PM_Searches, Trial_IDs)

# disconnect dbs
#dbDisconnect(eu_temp_db)
dbDisconnect(con)
