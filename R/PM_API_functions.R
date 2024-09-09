# Pubmed search functions

generate_esearch_url <- function(search_term, api_object,
                                 mindate = Sys.Date() - 1,
                                 maxdate = Sys.Date() - 1) {
  paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed",
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
get_esearch_results <- function(search_term, api_object,
                                mindate = Sys.Date() - 1,
                                maxdate = Sys.Date() - 1) {
  Sys.sleep(0.1)

  generate_esearch_url(search_term, api_object, mindate = mindate, maxdate = maxdate) |>
    httr::GET() |>
    httr::content()
}
generate_efetch_url <- function(pmid, api_object) {
  paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed",
    "&api_key",
    api_object,
    "&id=",
    pmid
  )
}
get_efetch_results <- function(pmid, api_object) {
  Sys.sleep(0.1)

  generate_efetch_url(pmid, api_object) |>
    httr::GET() |>
    httr::content()
}
return_pm_df <- function(single_id_query_results, search_term) {
  tibble::tibble(
    "ID" = search_term,
    "Query_Date" = Sys.Date(),
    "pmid" = xml2::xml_text(xml2::xml_find_first(single_id_query_results, ".//PMID")),
    "doi" = paste0("https://doi.org/", xml2::xml_text(xml2::xml_find_first(single_id_query_results, ".//ArticleId[@IdType='doi']"))),
    "title" = xml2::xml_text(xml2::xml_find_all(single_id_query_results, ".//ArticleTitle")),
    "abstract" = xml2::xml_text(xml2::xml_find_all(single_id_query_results, ".//Abstract")),
    "jabbrv" = xml2::xml_text(xml2::xml_find_first(single_id_query_results, ".//ISOAbbreviation")),
    "journal" = xml2::xml_text(xml2::xml_find_first(single_id_query_results, ".//Title"))
  )
}
get_efetch_results_in_tibble_form <- function(pmid, api_object, search_term) {
  get_efetch_results(pmid, api_object) |>
    return_pm_df(search_term = search_term)
}
generate_pm_tibble_from_search_term <- function(search_term, api_object, mindate = Sys.Date() - 1, maxdate = Sys.Date() - 1) {
  id_results <- get_esearch_results(search_term, api_object = api_object, mindate = mindate, maxdate = maxdate)
  id_list <- id_results$esearchresult$idlist |> unlist()

  purrr::map(id_list, get_efetch_results_in_tibble_form, api_object = api_object, search_term = search_term) |> dplyr::bind_rows()
}
generate_pm_tibble_from_search_term_series <- function(search_term_list, api_object, mindate = Sys.Date() - 1, maxdate = Sys.Date() - 1) {
  purrr::map(search_term_list, generate_pm_tibble_from_search_term, api_object = api_object, mindate = mindate, maxdate = maxdate) |>
    dplyr::bind_rows()
}
