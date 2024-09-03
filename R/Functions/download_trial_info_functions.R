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

#API call generating functions
#Generate NCT URLS
generate_NCT_URL <- function(NCT_ID_Vec){
  
  api_url <- paste0(
    "https://clinicaltrials.gov/api/v2/studies?format=json&filter.ids=",
    NCT_ID_Vec,
    "&fields=",
    paste("NCTId", "OrgStudyId", "Condition", 
          "BriefTitle", "Acronym",
          "OverallStatus", "PrimaryCompletionDate",
          "CompletionDate", "ResultsFirstSubmitDate", "ResultsFirstPostDate",
          "LastUpdatePostDate", "SeeAlsoLinkURL",
          sep = "%7C"),
    "&pageSize=1000")
  
  return(api_url)
  
}

#Return NCT Dataframe
#verbose = true because for some reason this fails on linux otherwise....
generate_NCT_DF <- function(url){
  
  df <- url %>% 
    GET(verbose = TRUE) %>% 
    content() %>% 
    tibble() %>% 
    head(1) %>% 
    rename(studies = 1) %>% 
    unnest_longer(studies) %>% 
    unnest_wider(studies) %>% 
    unnest_wider(protocolSection) %>% 
    unnest_wider(c(identificationModule, statusModule, conditionsModule)) %>% 
    unnest_wider(orgStudyIdInfo) %>% 
    hoist(primaryCompletionDateStruct,
          PrimaryCompletionDate = "date") %>% 
    hoist(completionDateStruct,
          CompletionDate = "date") %>% 
    hoist(resultsFirstPostDateStruct,
          ResultsFirstPostDate = "date") %>% 
    hoist(lastUpdatePostDateStruct,
          LastUpdatePostDate = "date") %>%
    hoist(referencesModule,
          SeeAlsoLinkURL = "url") %>%
    unnest_wider(referencesModule) %>% 
    rowwise() %>% 
    mutate(Condition = str_c(unlist(conditions), 
                             collapse = "|"),
           SeeAlsoLinkURL = str_c(unlist(seeAlsoLinks), 
                                  collapse = "|"),
           OverallStatus = str_to_sentence(str_replace_all(overallStatus, "_", " ")),
           OverallStatus = str_replace(OverallStatus, "Active not recruiting", "Active, not recruiting"),
           OverallStatus = str_replace(OverallStatus, "Unknown", "Unknown status"),
           .keep = "unused") %>% 
    ungroup() %>% 
    arrange(desc(nctId)) %>% 
    mutate(Rank = row_number(), .before = 1) %>% 
    janitor::clean_names("big_camel") %>% 
    rename(NCTId = NctId,
           OrgStudyId = Id,
           SeeAlsoLinkURL = SeeAlsoLinkUrl) %>% 
    select(Rank, NCTId, OrgStudyId, Condition, BriefTitle, Acronym, OverallStatus, everything()) %>% 
    relocate(LastUpdatePostDate, .before = SeeAlsoLinkURL)
  
  return(df)
}

# Generate ISRCTN Dataframe
generate_ISRCTN_df <- function(ISRCTN_URL){
  
  ISRCTN_XML <- GET(ISRCTN_URL) %>% content(encoding = 'UTF-8')
  
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