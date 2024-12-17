#' Generate ESearch URL
#'
#' This function generates the ESearch URL for PubMed search.
#'
#' @param search_term A character string specifying the search term.
#' @param api_object A character string specifying the API key.
#' @param mindate The start date for the search. Defaults to yesterday's date.
#' @param maxdate The end date for the search. Defaults to yesterday's date.
#' @return A character string containing the ESearch URL.
#' @export
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

#' Get ESearch Results
#'
#' This function retrieves ESearch results from PubMed.
#'
#' @param search_term A character string specifying the search term.
#' @param api_object A character string specifying the API key.
#' @param mindate The start date for the search. Defaults to yesterday's date.
#' @param maxdate The end date for the search. Defaults to yesterday's date.
#' @return A list containing the ESearch results.
#' @importFrom httr2 request req_perform resp_body_json
#' @export
get_esearch_results <- function(search_term, api_object,
                                mindate = Sys.Date() - 1,
                                maxdate = Sys.Date() - 1) {

  Sys.sleep(0.1) # sleep to prevent too many API hits

  url <- generate_esearch_url(search_term, api_object, mindate = mindate, maxdate = maxdate)
  request <- httr2::request(url)
  response <- httr2::req_perform(request)
  httr2::resp_body_json(response)
}

#' Generate EFetch URL
#'
#' This function generates the EFetch URL for PubMed search.
#'
#' @param pmid A character string specifying the PubMed ID.
#' @param api_object A character string specifying the API key.
#' @return A character string containing the EFetch URL.
#' @export
generate_efetch_url <- function(pmid, api_object) {
  paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed",
    "&api_key=",
    api_object,
    "&id=",
    pmid
  )
}

#' Get EFetch Results
#'
#' This function retrieves EFetch results from PubMed.
#'
#' @param pmid A character string specifying the PubMed ID.
#' @param api_object A character string specifying the API key.
#' @return A list containing the EFetch results.
#' @importFrom httr2 request req_perform resp_body_xml
#' @export
get_efetch_results <- function(pmid, api_object) {
  Sys.sleep(0.1)

  url <- generate_efetch_url(pmid, api_object)
  request <- httr2::request(url)
  response <- httr2::req_perform(request)
  httr2::resp_body_xml(response)
}

#' Return PubMed Dataframe
#'
#' This function returns a dataframe from PubMed query results.
#'
#' @param single_id_query_results A list containing the query results for a single ID.
#' @param search_term A character string specifying the search term.
#' @return A tibble dataframe containing the PubMed results.
#' @importFrom tibble tibble
#' @importFrom xml2 xml_text xml_find_first xml_find_all
#' @export
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

#' Get EFetch Results in Tibble Form
#'
#' This function retrieves EFetch results and returns them in tibble form.
#'
#' @param pmid A character string specifying the PubMed ID.
#' @param api_object A character string specifying the API key.
#' @param search_term A character string specifying the search term.
#' @return A tibble dataframe containing the PubMed results.
#' @export
get_efetch_results_in_tibble_form <- function(pmid, api_object, search_term) {
  get_efetch_results(pmid, api_object) |>
    return_pm_df(search_term = search_term)
}

#' Generate PubMed Tibble from Search Term
#'
#' This function generates a tibble dataframe from a PubMed search term.
#'
#' @param search_term A character string specifying the search term.
#' @param api_object A character string specifying the API key.
#' @param mindate The start date for the search. Defaults to yesterday's date.
#' @param maxdate The end date for the search. Defaults to yesterday's date.
#' @return A tibble dataframe containing the PubMed results.
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @export
generate_pm_tibble_from_search_term <- function(search_term, api_object, mindate = Sys.Date() - 1, maxdate = Sys.Date() - 1) {

  id_results <- get_esearch_results(search_term, api_object = api_object, mindate = mindate, maxdate = maxdate)
  id_list <- id_results$esearchresult$idlist |> unlist()
  result <- purrr::map(id_list, get_efetch_results_in_tibble_form, api_object = api_object, search_term = search_term) |> dplyr::bind_rows()

  return(result)

}

#' Generate PubMed Tibble from Search Term Series
#'
#' This function generates a tibble dataframe from a series of PubMed search terms.
#'
#' @param search_term_list A list of character strings specifying the search terms.
#' @param api_object A character string specifying the API key.
#' @param mindate The start date for the search. Defaults to yesterday's date.
#' @param maxdate The end date for the search. Defaults to yesterday's date.
#' @return A tibble dataframe containing the PubMed results.
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @export
generate_pm_tibble_from_search_term_series <- function(search_term_list, api_object, mindate = Sys.Date() - 1, maxdate = Sys.Date() - 1) {
  purrr::map(search_term_list, generate_pm_tibble_from_search_term, api_object = api_object, mindate = mindate, maxdate = maxdate) |>
    dplyr::bind_rows()
}
