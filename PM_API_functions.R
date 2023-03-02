#Pubmed search functions
library(httr)
library(dplyr)
library(purrr)
library(xml2)

generate_esearch_url <- function(search_term, api_object, mindate = Sys.Date()-1, maxdate = Sys.Date()-1){
  
  paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed",
         "&mindate=",
         mindate,
         "&maxdate=",
         maxdate,
         "&datetype=edat&retmode=json&api_key=",
         api_object,
         "&term=",
         search_term
  )
  
}
get_esearch_results <- function(search_term, api_object, mindate = Sys.Date()-1, maxdate = Sys.Date()-1){
  
  Sys.sleep(0.1)
  
  generate_esearch_url(search_term, api_object, mindate = mindate, maxdate = maxdate) %>% 
    GET() %>% 
    content()
  
}
generate_efetch_url <- function(pmid, api_object){
  
  paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed",
         "&api_key",
         api_object,
         "&id=",
         pmid)
  
}
get_efetch_results <- function(pmid, api_object){
  
  Sys.sleep(0.1)
  
  generate_efetch_url(pmid, api_object) %>% 
    GET() %>% 
    content()
  
}
return_pm_df <- function(single_id_query_results, search_term){
  
  tibble("ID" = search_term,
         "Query_Date" = Sys.Date(),
         "pmid" = xml_text(xml_find_first(single_id_query_results, ".//PMID")),
         "doi" = paste0("https://doi.org/",xml_text(xml_find_first(single_id_query_results, ".//ArticleId[@IdType='doi']"))),
         "title" = xml_text(xml_find_all(single_id_query_results, ".//ArticleTitle")),
         "abstract" = xml_text(xml_find_all(single_id_query_results, ".//Abstract")),
         "jabbrv" = xml_text(xml_find_first(single_id_query_results, ".//ISOAbbreviation")),
         "journal" = xml_text(xml_find_first(single_id_query_results, ".//Title"))
         )
  
}
get_efetch_results_in_tibble_form <- function(pmid, api_object, search_term){
  
  get_efetch_results(pmid, api_object) %>% 
    return_pm_df(search_term = search_term)
  
}
generate_pm_tibble_from_search_term <- function(search_term, api_object, mindate = Sys.Date()-1, maxdate = Sys.Date()-1){
  
  id_results <- get_esearch_results(search_term, api_object = api_object, mindate = mindate, maxdate = maxdate)
  id_list <- id_results$esearchresult$idlist %>% as_vector()
  
  map(id_list, get_efetch_results_in_tibble_form, api_object = api_object, search_term = search_term) %>% bind_rows()
  
}
generate_pm_tibble_from_search_term_series <- function(search_term_list, api_object, mindate = Sys.Date()-1, maxdate = Sys.Date()-1){
  
  map(search_term_list, generate_pm_tibble_from_search_term, api_object = api_object, mindate = mindate, maxdate = maxdate) %>% 
    bind_rows()
  
}