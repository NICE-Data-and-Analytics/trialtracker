#' Clean UTF strings
#'
#' This internal function cleans UTF-8 strings
#'
#' @param x A string to be cleaned
#' @return A cleaned string
#' @keywords internal
clean_UTF <- function(x) {
  iconv(x, "UTF-8", "UTF-8", sub = "")
}

#' Concatenate Trial IDs for API URL Construction
#'
#' This internal function concatenates Trial IDs from a specified column in a dataframe, ensuring no missing or duplicate values. It is used for constructing API URLs.
#'
#' @param df A dataframe containing the IDs.
#' @param id_col A string specifying the column name that contains the IDs.
#' @return A vector of unique, non-missing IDs.
#' @importFrom dplyr select filter distinct pull all_of everything
#' @importFrom tidyr drop_na
#' @keywords internal
concat_ids <- function(df, id_col) {

  if (!id_col %in% colnames(df)) {
    stop(paste("Column", id_col, "not found in dataframe"))
  }

  df <- df |>
    dplyr::select(dplyr::all_of(id_col)) |>
    tidyr::drop_na()

  if (nrow(df) > 0) {
    return(
      df |>
        dplyr::filter(dplyr::if_any(.cols = dplyr::everything(), .fns = ~ (. != ""))) |>
        dplyr::distinct() |>
        dplyr::pull()
    )
  } else {
    return(
      df |>
        dplyr::distinct() |>
        dplyr::pull()
    )
  }
}

#' Collapse IDs for API URL Construction
#'
#' This internal function collapses IDs from a specified column in a dataframe into a single string, separated by a specified delimiter. It is used for constructing API URLs.
#'
#' @param df A dataframe containing the IDs.
#' @param id_col A string specifying the column name that contains the IDs.
#' @param paste_collapse A string to use as the delimiter for collapsing the IDs.
#' @return A single string of concatenated IDs separated by the specified delimiter.
#' @keywords internal
collapse_ids <- function(df, id_col, paste_collapse) {
  concat_ids(df, id_col) |>
    paste0(collapse = paste_collapse)
}

#' Create a List of IDs for API Call
#'
#' This internal function generates a list of unique, non-missing IDs from a specified column in a dataframe. It is used to prepare IDs for inclusion in an API call.
#'
#' @param trial_id_df A dataframe containing the trial IDs.
#' @param registry A string specifying the registry name, which is used to construct the column name containing the IDs.
#' @return A vector of unique, non-missing IDs.
#' @importFrom dplyr select distinct if_any filter pull all_of everything
#' @importFrom tidyr drop_na
#' @keywords internal
create_search_list <- function(trial_id_df, registry) {
  id_vector <- paste0(registry, "_Ids")

  if (!id_vector %in% colnames(trial_id_df)) {
    stop(paste("Column", id_vector, "not found in dataframe"))
  }

  trial_id_df |>
    dplyr::select(dplyr::all_of(id_vector)) |>
    dplyr::distinct() |>
    tidyr::drop_na() |>
    dplyr::filter(dplyr::if_any(.cols = dplyr::everything(), .fns = ~ (. != ""))) |>
    dplyr::pull()
}

#' Updates the SQLite Database with API Call Information
#'
#' This function updates the specified SQLite database table with new data from an API call. If the table does not exist, it creates the table and appends the data. If the table exists, it appends the data only if there are no entries for the current date. If entries for the current date exist, it overwrites the table with entries from previous dates and then appends the new data.
#'
#' @param main_con A DBI connection object to the SQLite database.
#' @param registry A string specifying the name of the registry (table) to update.
#' @param DF A dataframe containing the data to be inserted into the database.
#' @return None. This function is called for its side effects.
#' @importFrom DBI dbListTables dbCreateTable dbAppendTable dbWriteTable
#' @export
#' @examples
#' \dontrun{
#' main_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' DF <- data.frame(Query_Date = Sys.Date(), Value = 1:5)
#' update_db(main_con, "example_registry", DF)
#' }
update_db <- function(main_con, registry, DF) {
  if (!is.null(DF)) {
    if (!(registry %in% DBI::dbListTables(main_con))) {
      DBI::dbCreateTable(main_con, registry, DF)
      DBI::dbAppendTable(main_con, registry, DF)
    } else {
      existing_data <- DBI::dbReadTable(main_con, registry)
      if (nrow(existing_data[existing_data$Query_Date == Sys.Date(), ]) == 0) {
        DBI::dbAppendTable(main_con, registry, DF)
      } else {
        temp_df <- existing_data[existing_data$Query_Date < Sys.Date(), ]
        DBI::dbWriteTable(main_con, registry, temp_df, overwrite = TRUE)
        DBI::dbAppendTable(main_con, registry, DF)
      }
    }
  }
}

#' Generate URL for API Call to ClinicalTrials.gov
#'
#' This function generates a URL for an API call to ClinicalTrials.gov from a vector of Trial IDs. The URL is constructed to retrieve specific fields for the given trial IDs in JSON format.
#'
#' @param NCT_ID_Vec A character vector of ClinicalTrials.gov Trial IDs.
#' @return A string containing the URL for the API call.
#' @export
#' @examples
#' \dontrun{
#' trial_ids <- c("NCT03146143", "NCT00408044", "NCT01377987")
#' url <- generate_NCT_URL(trial_ids)
#' print(url)
#' }
generate_NCT_URL <- function(NCT_ID_Vec) {

  #Check vector unique and no NAs
  NCT_ID_Vec <- unique(NCT_ID_Vec)
  NCT_ID_Vec <- NCT_ID_Vec[!is.na(NCT_ID_Vec)]

  concat_vector <- paste0(NCT_ID_Vec, collapse = "%7C")

  api_url <-
    paste0(
      "https://clinicaltrials.gov/api/v2/studies?format=json&filter.ids=",
      concat_vector,
      "&fields=",
      paste("NCTId", "OrgStudyId", "Condition",
        "BriefTitle", "Acronym",
        "OverallStatus", "PrimaryCompletionDate",
        "CompletionDate", "ResultsFirstSubmitDate", "ResultsFirstPostDate",
        "LastUpdatePostDate", "SeeAlsoLinkURL",
        sep = "%7C"
      ),
      "&pageSize=1000"
    )

  return(api_url)
}

#' Generate Formatted Tibble from ClinicalTrials.gov API Call URL
#'
#' This function returns a formatted tibble from a given ClinicalTrials.gov API call URL. The tibble includes various fields such as NCT ID, study ID, condition, brief title, acronym, overall status, and more.
#'
#' @param url A string containing the URL for the API call to ClinicalTrials.gov.
#' @return A tibble containing the formatted data from the API response.
#' @importFrom dplyr na_if arrange desc mutate row_number
#' @importFrom tidyr unnest
#' @importFrom purrr map_chr pluck
#' @importFrom jsonlite read_json
#' @importFrom tibble tibble
#' @importFrom stringr str_c str_replace_all str_to_sentence str_replace
#' @export
#' @examples
#' \dontrun{
#' api_url <- "https://clinicaltrials.gov/api/v2/studies?format=json&filter.ids=NCT03146143%7CNCT00408044"
#' api_fields <- "&fields=NCTId%7COrgStudyId%7CCondition%7CBriefTitle%7CAcronym%7COverallStatus%7CPrimaryCompletionDate%7CCompletionDate%7CResultsFirstSubmitDate%7CResultsFirstPostDate%7CLastUpdatePostDate%7CSeeAlsoLinkURL&pageSize=1000"
#' df <- generate_NCT_DF(c(api_url, api_fields))
#' print(df)
#' }
generate_NCT_DF <- function(url) {
  tryCatch({
    df_resp <- url |>
      jsonlite::read_json()

    studies <- df_resp$studies

    ncts <- purrr::map_chr(studies,
                           \(x) purrr::pluck(x, "protocolSection", "identificationModule", "nctId", .default = NA)
    )
    orgstudyid <- purrr::map_chr(studies,
                                 \(x) purrr::pluck(x, "protocolSection", "identificationModule", "orgStudyIdInfo", "id",
                                                   .default = NA))
    conditions <- purrr::map_chr(studies,
                                 \(x) purrr::pluck(x, "protocolSection", "conditionsModule", "conditions") |>
                                   stringr::str_c(collapse = "|") |>
                                   dplyr::na_if("")
    )
    briefTitles <- purrr::map_chr(studies,
                                  \(x) purrr::pluck(x, "protocolSection", "identificationModule", "briefTitle", .default = NA))
    acronyms <- purrr::map_chr(studies,
                               \(x) purrr::pluck(x, "protocolSection", "identificationModule", "acronym", .default = NA))
    overallstatuses <- purrr::map_chr(studies,
                                      \(x) purrr::pluck(x, "protocolSection", "statusModule", "overallStatus", .default = NA) |>
                                        stringr::str_replace_all("_", " ") |>
                                        stringr::str_to_sentence() |>
                                        stringr::str_replace("Active not recruiting", "Active, not recruiting") |>
                                        stringr::str_replace("Unknown", "Unknown status")
    )
    primarycompletiondates <- purrr::map_chr(studies,
                                             \(x) purrr::pluck(x, "protocolSection", "statusModule",
                                                               "primaryCompletionDateStruct", "date", .default = NA))
    completiondates <- purrr::map_chr(studies,
                                      \(x) purrr::pluck(x, "protocolSection", "statusModule",
                                                        "completionDateStruct", "date", .default = NA))
    resultsfirstsubmitdates <- purrr::map_chr(studies,
                                              \(x) purrr::pluck(x, "protocolSection", "statusModule",
                                                                "resultsFirstSubmitDate", .default = NA))
    resultsfirstpostdates <- purrr::map_chr(studies,
                                            \(x) purrr::pluck(x, "protocolSection", "statusModule",
                                                              "resultsFirstPostDateStruct", "date", .default = NA))
    lastupdatepostdates <- purrr::map_chr(studies,
                                          \(x) purrr::pluck(x, "protocolSection", "statusModule",
                                                            "lastUpdatePostDateStruct", "date", .default = NA))

    seealsolinks <- purrr::map_chr(studies,
                                   \(x) purrr::pluck(x, "protocolSection", "referencesModule", "seeAlsoLinks", 1, "url",
                                                     .default = NA) |> stringr::str_c(collapse = "|"))

    df <- tibble::tibble("NCTId" = ncts,
                         "OrgStudyId" = orgstudyid,
                         "Condition" = conditions,
                         "BriefTitle" = briefTitles,
                         "Acronym" = acronyms,
                         "OverallStatus" = overallstatuses,
                         "PrimaryCompletionDate" = primarycompletiondates,
                         "CompletionDate" = completiondates,
                         "ResultsFirstSubmitDate" = resultsfirstsubmitdates,
                         "ResultsFirstPostDate" = resultsfirstpostdates,
                         "LastUpdatePostDate" = lastupdatepostdates,
                         "SeeAlsoLinkURL" = seealsolinks) |>
      dplyr::arrange(dplyr::desc(NCTId)) |>
      dplyr::mutate(Rank = dplyr::row_number(), .before = 1)

    return(df)
  }, error = function(e) {
    message("Error fetching data from ClinicalTrials.gov API: ", e$message)
    # Return an empty tibble with the same structure
    tibble::tibble(
      "NCTId" = character(),
      "OrgStudyId" = character(),
      "Condition" = character(),
      "BriefTitle" = character(),
      "Acronym" = character(),
      "OverallStatus" = character(),
      "PrimaryCompletionDate" = character(),
      "CompletionDate" = character(),
      "ResultsFirstSubmitDate" = character(),
      "ResultsFirstPostDate" = character(),
      "LastUpdatePostDate" = character(),
      "SeeAlsoLinkURL" = character(),
      "Rank" = integer()
    )
  })
}

#' Generate ISRCTN API URL
#'
#' This function creates a URL for an ISRCTN API call using a vector of ISRCTN trial IDs.
#'
#' @param ISRCTN_Id_Vector A character vector of ISRCTN trial IDs.
#' @return A character string containing the URL for the ISRCTN API call.
#' @examples
#' generate_ISRCTN_URL(c("ISRCTN10985036", "ISRCTN25778550"))
#' @export
generate_ISRCTN_URL <- function(ISRCTN_Id_Vector) {

  if (length(ISRCTN_Id_Vector) == 0) {
    stop("The input vector is empty. Please provide at least one ISRCTN trial ID.")
  }

  paste0(
    "http://www.isrctn.com/api/query/format/who?q=",
    paste0(ISRCTN_Id_Vector, collapse = "%20OR%20"),
    "&limit=",
    length(ISRCTN_Id_Vector) + 10
  )
}

#' Generate ISRCTN Dataframe
#'
#' This function retrieves ISRCTN trial data from a given URL and returns it as a dataframe.
#'
#' @param ISRCTN_URL A character string specifying the URL to fetch the ISRCTN XML data.
#' @return A tibble dataframe containing the ISRCTN trial data.
#' @importFrom httr2 request req_perform resp_body_xml
#' @importFrom xml2 xml_text xml_find_all
#' @importFrom tibble tibble
#' @export
#' @examples
#' \dontrun{
#' ISRCTN_URL <- "https://www.isrctn.com/api/query/format/who?q="
#' ISRCTN_query <- "ISRCTN27106947%20OR%20ISRCTN16033549%20OR%20ISRCTN17573805%20OR%20ISRCTN73327972&limit=15"
#' df <- generate_ISRCTN_df(c(ISRCTN_URL, ISRCTN_query)
#' }
generate_ISRCTN_df <- function(ISRCTN_URL) {
  tryCatch({
    ISRCTN_XML <- httr2::request(ISRCTN_URL) |>
      httr2::req_perform() |>
      httr2::resp_body_xml()

    tibble::tibble(
      "ISRCTN_No" = xml2::xml_text(xml2::xml_find_all(ISRCTN_XML, ".//trial_id")),
      "Public_Title" = xml2::xml_text(xml2::xml_find_all(ISRCTN_XML, ".//public_title")),
      "Acronym" = xml2::xml_text(xml2::xml_find_all(ISRCTN_XML, ".//acronym")),
      "Scientific_Title" = xml2::xml_text(xml2::xml_find_all(ISRCTN_XML, ".//scientific_title")),
      "URL" = xml2::xml_text(xml2::xml_find_all(ISRCTN_XML, ".//url")),
      "Recruitment_Status" = xml2::xml_text(xml2::xml_find_all(ISRCTN_XML, ".//recruitment_status")),
      "Results_date_completed" = xml2::xml_text(xml2::xml_find_all(ISRCTN_XML, ".//results_date_completed")),
      "Results_url_link" = xml2::xml_text(xml2::xml_find_all(ISRCTN_XML, ".//results_url_link")),
      "Results_summary" = xml2::xml_text(xml2::xml_find_all(ISRCTN_XML, ".//results_summary")),
      "Results_date_posted" = xml2::xml_text(xml2::xml_find_all(ISRCTN_XML, ".//results_date_posted")),
      "Results_date_first_publication" = xml2::xml_text(xml2::xml_find_all(ISRCTN_XML, ".//results_date_first_publication"))
    )
  }, error = function(e) {
    message("Error fetching data from ISRCTN API: ", e$message)
    # Return an empty tibble with the same structure
    tibble::tibble(
      "ISRCTN_No" = character(),
      "Public_Title" = character(),
      "Acronym" = character(),
      "Scientific_Title" = character(),
      "URL" = character(),
      "Recruitment_Status" = character(),
      "Results_date_completed" = character(),
      "Results_url_link" = character(),
      "Results_summary" = character(),
      "Results_date_posted" = character(),
      "Results_date_first_publication" = character()
    )
  })
}

#' Update PubMed Tables for One Registry
#'
#' This function generates PubMed results from trial ids and updates the database for a specified registry.
#'
#' @param registry A character string specifying the registry (e.g., "ISRCTN", "NIHR").
#' @param trial_id_df The dataframe containing trial IDs.
#' @param main_con A database connection object. Defaults to a connection to the TrialTracker SQLite database.
#' @param mindate The start date for the PubMed search. Defaults to yesterday's date.
#' @param maxdate The end date for the PubMed search. Defaults to yesterday's date.
#' @return None. The function updates the database with the PubMed results.
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
#' @importFrom readr read_file
#' @importFrom stringr str_subset
#' @importFrom dplyr left_join select everything all_of distinct
#' @export
generate_pubmed_results_from_search_terms_and_update_db_one_registry <- function(registry,
                                                                                 trial_id_df,
                                                                                 main_con = DBI::dbConnect(RSQLite::SQLite(), "inst/extdata/RSQLite_data/TrialTracker-db.sqlite"),
                                                                                 mindate = Sys.Date() - 1,
                                                                                 maxdate = Sys.Date() - 1) {
  api <- readr::read_file("secrets/entrez.key")

  registry_id <- paste0(registry, "_Ids")
  registry_id_cols_to_remove <- c("ISRCTN_Ids", "NIHR_Ids", "EU_Ids", "NCT_Ids") |>
    stringr::str_subset(registry_id, negate = TRUE)
  registry_db_pm_name <- paste0(registry, "_PM")

  pm_tibble <- create_search_list(trial_id_df = trial_id_df, registry = registry) |>
    generate_pm_tibble_from_search_term_series(
      api_object = api,
      mindate = mindate,
      maxdate = maxdate
    )

  if (nrow(pm_tibble) > 0) {
    pm_tibble <- pm_tibble |>
      dplyr::left_join(trial_id_df, by = c("ID" = registry_id)) |>
      dplyr::select(Program, Guideline.number, dplyr::everything()) |>
      dplyr::select(-dplyr::all_of(registry_id_cols_to_remove)) |>
      dplyr::distinct()

    update_db(main_con, registry_db_pm_name, pm_tibble)
  }
}

#' Update All PubMed Tables
#'
#' This wrapper function updates PubMed tables for all specified registries.
#'
#' @param registries A character vector of registry names. Defaults to c("NCT", "ISRCTN", "NIHR", "EU").
#' @param trial_id_df A dataframe containing trial IDs.
#' @param main_con A database connection object.
#' @return None. The function updates the PubMed tables in the database.
#' @importFrom purrr walk
#' @export
update_all_pubmed_tables <- function(registries = c("NCT", "ISRCTN", "NIHR", "EU"), trial_id_df, main_con) {
  purrr::walk(registries, generate_pubmed_results_from_search_terms_and_update_db_one_registry, trial_id_df = trial_id_df, main_con = main_con)
}

#' Update Database for NCT Registry Changes
#'
#' This function updates the database for changes in the clinicaltrials.gov registry.
#'
#' @param main_con A database connection object.
#' @param trial_id_df A dataframe containing trial IDs.
#' @return None. The function updates the NCT registry data in the database.
#' @importFrom dplyr bind_rows right_join filter mutate select distinct
#' @export
update_db_for_NCT_changes <- function(main_con,
                                      trial_id_df) {
  # Collapse Trial ID numbers into search term
  #print(trial_id_df)
  half_NCT_Id_Vector <- round(length(concat_ids(trial_id_df, "NCT_Ids")) / 2, 0)
  NCT_Id_Vector1 <- concat_ids(trial_id_df, "NCT_Ids")[1:half_NCT_Id_Vector]
  NCT_Id_Vector2 <- concat_ids(trial_id_df, "NCT_Ids")[(half_NCT_Id_Vector + 1):length(concat_ids(trial_id_df, "NCT_Ids"))]

  # Construct URLs

  # The clinicalTrial.gov API can handle 1000 request at a time. Above 1000 and the extract will need to be split.
  # Split URL into 2 as URL too long now

  NCT_URL1 <- generate_NCT_URL(NCT_Id_Vector1)
  NCT_URL2 <- generate_NCT_URL(NCT_Id_Vector2)

  # Results DFs
  NCT_DF1 <- generate_NCT_DF(NCT_URL1)
  NCT_DF2 <- generate_NCT_DF(NCT_URL2)

  NCT_DF <- dplyr::bind_rows(NCT_DF1, NCT_DF2) |>
    dplyr::right_join(trial_id_df[, c("Program", "Guideline.number", "URL", "NCT_Ids")],
                      by = c("NCTId" = "NCT_Ids"), multiple = "all"
    ) |>
    dplyr::filter(!is.na(NCTId)) |>
    dplyr::mutate(Query_Date = Sys.Date()) |>
    dplyr::select(Query_Date, Program, Guideline.number, URL, everything(), -Rank) |>
    dplyr::distinct() # remove any duplicates

  update_db(main_con, "NCT", NCT_DF)
}

#' Update Database for ISRCTN Registry Changes
#'
#' This function updates the database for changes in the ISRCTN registry.
#'
#' @param main_con A database connection object.
#' @param trial_id_df A dataframe containing trial IDs.
#' @return None. The function updates the ISRCTN registry data in the database.
#' @importFrom dplyr bind_rows right_join filter mutate select distinct
#' @export
update_db_for_ISRCTN_changes <- function(main_con,
                                         trial_id_df) {
  # Collapse trial ids into search terms
  half_ISRCTN_Id_Vector <- round(length(concat_ids(trial_id_df, "ISRCTN_Ids")) / 2, 0)
  ISRCTN_Id_Vector1 <- concat_ids(trial_id_df, "ISRCTN_Ids")[1:half_ISRCTN_Id_Vector]
  ISRCTN_Id_Vector2 <- concat_ids(trial_id_df, "ISRCTN_Ids")[(half_ISRCTN_Id_Vector + 1):length(concat_ids(trial_id_df, "ISRCTN_Ids"))] |> paste0(collapse = "%20OR%20")

  # ISRCTN URLs done in two parts as v large
  ISRCTN_URL1 <- generate_ISRCTN_URL(ISRCTN_Id_Vector1)
  ISRCTN_URL2 <- generate_ISRCTN_URL(ISRCTN_Id_Vector2)

  # Create intermediate dfs
  ISRCTN_DF1 <- generate_ISRCTN_df(ISRCTN_URL1)
  ISRCTN_DF2 <- generate_ISRCTN_df(ISRCTN_URL2)

  # Create final df
  ISRCTN_DF <- dplyr::bind_rows(ISRCTN_DF1, ISRCTN_DF2) |>
    dplyr::right_join(trial_id_df[, c("Program", "Guideline.number", "ISRCTN_Ids")], by = c("ISRCTN_No" = "ISRCTN_Ids"), multiple = "all") |>
    dplyr::filter(!is.na(ISRCTN_No)) |>
    dplyr::mutate(Query_Date = Sys.Date()) |>
    dplyr::select(Query_Date, Program, Guideline.number, URL, everything()) |>
    dplyr::distinct()

  # Update db
  update_db(main_con, "ISRCTN", ISRCTN_DF)
}

#' Update Database for NIHR Registry Changes
#'
#' This function updates the database for changes in the NIHR registry.
#'
#' @param main_con A database connection object.
#' @param trial_id_df A dataframe containing trial IDs.
#' @return None. The function updates the NIHR registry data in the database.
#' @importFrom jsonlite fromJSON
#' @importFrom tidyr drop_na
#' @importFrom stringr str_replace_all
#' @export
update_db_for_NIHR_changes <- function(main_con, trial_id_df) {
  tryCatch({
    # Construct URL
    NIHR_URL_API2 <- "https://nihr.opendatasoft.com/api/v2/catalog/datasets/infonihr-open-dataset/exports/json?limit=-1&offset=0&lang=en&timezone=UTC"

    # Fetch JSON data from the API
    NIHR_json <- jsonlite::fromJSON(url(NIHR_URL_API2))

    # Process trial IDs
    NIHR_Trial_IDs <- trial_id_df |>
      dplyr::select(Program, Guideline.number, URL, NIHR_Ids) |>
      tidyr::drop_na(NIHR_Ids) |>
      dplyr::mutate("projectjoin" = stringr::str_replace_all(NIHR_Ids, "[^\\d]", ""))

    # Process NIHR data and join with trial IDs
    NIHR_DF <- NIHR_json |>
      dplyr::mutate("projectjoin" = stringr::str_replace_all(project_id, "[^\\d]", "")) |>
      dplyr::right_join(NIHR_Trial_IDs, by = c("projectjoin"), multiple = "all") |>
      tidyr::drop_na(projectjoin) |>
      dplyr::mutate(Query_Date = Sys.Date())  |>
      dplyr::select(Query_Date,
                    Program,
                    Guideline.number,
                    URL,
                    project_id,
                    project_title,
                    project_status,
                    project_id,
                    end_date) |>
      dplyr::distinct()

    # Update the database
    update_db(main_con, "NIHR", NIHR_DF)
  }, error = function(e) {
    # Handle the error
    message("Error fetching data from NIHR API: ", e$message)
  })
}

#' Update Database for EU Registry Changes
#'
#' This function updates the database for changes in the EU registry.
#'
#' @param main_con A database connection object.
#' @param trial_id_df A dataframe containing trial IDs.
#' @return None. The function updates the EU registry data in the database.
#' @importFrom dplyr mutate right_join filter select distinct rename
#' @importFrom httr2 request req_options req_perform resp_status
#' @importFrom ctrdata ctrLoadQueryIntoDb dbGetFieldsIntoDf dbFindFields
#' @importFrom stringr str_subset str_extract
#' @importFrom nodbi src_sqlite
#' @export
update_db_for_EU_changes <- function(main_con, trial_id_df) {
  #print("Inside update_db_for_EU_changes")

  # Create vector of EU Ids
  EU_Vector <- collapse_ids(trial_id_df, "EU_Ids", "+OR+")

  # Generate API URL
  EU_URL <- paste0(
    "https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
    EU_Vector
  )

  # Create a request object
  req <- httr2::request(EU_URL) |>
    httr2::req_options(ssl_verifypeer = FALSE)

  # Perform the request
  res <- httr2::req_perform(req)

  # Check for successful response
  if (httr2::resp_status(res) != 200) {
    stop("Failed to fetch data from EU registry")
  }

  # sqlite db
  eu_temp_db <- nodbi::src_sqlite(dbname = "inst/extdata/RSQLite_data/EU_temp_db.sqlite", collection = "EU")

  try(ctrdata::ctrLoadQueryIntoDb(queryterm = EU_URL, con = eu_temp_db))

  # Collapse fields into 1 vector
  EU_DF <- ctrdata::dbGetFieldsIntoDf(
    stringr::str_subset(
      c(
        ctrdata::dbFindFields("end_of_trial", con = eu_temp_db),
        ctrdata::dbFindFields("a3", con = eu_temp_db),
        ctrdata::dbFindFields("a4", con = eu_temp_db)
      ),
      "_it|_es|_fr|_nl",
      negate = TRUE
    ),
    con = eu_temp_db
  ) |>
    dplyr::mutate(EU_Ids = stringr::str_extract(`_id`, "^\\d{4}-\\d{6}-\\d{2}")) |>
    dplyr::right_join(trial_id_df, multiple = "all") |>
    dplyr::filter(!is.na(`_id`)) |>
    dplyr::mutate(Query_Date = Sys.Date()) |>
    dplyr::select(
      Query_Date, Program, Guideline.number, URL, everything(),
      -NCT_Ids, -ISRCTN_Ids, -NIHR_Ids,
      -Short..working.title.
    ) |>
    dplyr::rename("X_id" = `_id`) |>
    dplyr::distinct()

  update_db(main_con, "EU", EU_DF)

}

#' Wrapper Function for All Updates (No PubMed or Email)
#'
#' This wrapper function updates the database for all registries without updating PubMed tables or sending emails.
#'
#' @param main_con A database connection object. Defaults to a connection to the TrialTracker SQLite database.
#' @param trial_id_df A dataframe containing trial IDs. Defaults to reading from the Trial_IDs table in the database.
#' @return None. The function updates the database for all registries.
#' @importFrom DBI dbConnect dbReadTable
#' @importFrom RSQLite SQLite
#' @export
download_trial_info_wrapper_no_pm_or_email <- function(
    main_con = DBI::dbConnect(RSQLite::SQLite(), "inst/extdata/RSQLite_data/TrialTracker-db.sqlite"),
    trial_id_df = DBI::dbReadTable(main_con, "Trial_IDs")) {

  # Call update functions
  update_db_for_NCT_changes(main_con = main_con, trial_id_df = trial_id_df)
  update_db_for_ISRCTN_changes(main_con = main_con, trial_id_df = trial_id_df)
  update_db_for_NIHR_changes(main_con = main_con, trial_id_df = trial_id_df)
  update_db_for_EU_changes(main_con = main_con, trial_id_df = trial_id_df)

    # Re-render the Quarto file
  quarto::quarto_render("index.qmd")

}

#' Wrapper Function for All Updates
#'
#' This wrapper function updates the database for all registries and also updates PubMed tables and sends email alerts.
#'
#' @param main_con A database connection object. Defaults to a connection to the TrialTracker SQLite database.
#' @param trial_id_df A dataframe containing trial IDs. Defaults to reading from the Trial_IDs table in the database.
#' @param dev_flag A development flag (logical TRUE / FALSE) toggling whether emails go to dev team only (TRUE) or dashboard users and dev team (FALSE).
#' @return None. The function updates the database for all registries, updates PubMed tables, and sends email alerts.
#' @importFrom DBI dbConnect dbReadTable
#' @importFrom RSQLite SQLite
#' @export
download_trial_info_wrapper <- function(main_con = DBI::dbConnect(RSQLite::SQLite(), "inst/extdata/RSQLite_data/TrialTracker-db.sqlite"),
                                        trial_id_df = DBI::dbReadTable(main_con, "Trial_IDs"),
                                        dev_flag) {
  download_trial_info_wrapper_no_pm_or_email()
  update_all_pubmed_tables(main_con = main_con, trial_id_df = trial_id_df)
  generate_email_alerts(dev_flag = dev_flag)

  # Re-render the Quarto file
  quarto::quarto_render("index.qmd")

}
