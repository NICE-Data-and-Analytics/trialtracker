# Concatenation functions used for constructing API urls
concat_ids <- function(df, id_col) {
  df <- df |>
    dplyr::select(tidyselect::all_of(id_col)) |>
    tidyr::drop_na()

  if (nrow(df) > 0) {
    return(
      df |>
        dplyr::filter(dplyr::if_any(.cols = everything(), .fns = ~ (. != ""))) |>
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
collapse_ids <- function(df, id_col, paste_collapse) {
  concat_ids(df, id_col) |>
    paste0(collapse = paste_collapse)
}

# Create a list of ids to search
create_search_list <- function(trial_id_df, registry) {
  id_vector <- paste0(registry, "_Ids")

  trial_id_df |>
    dplyr::select(all_of(id_vector)) |>
    dplyr::distinct() |>
    tidyr::drop_na() |>
    dplyr::pull()
}

# Updates the db with info
update_db <- function(con, registry, DF) {
  if (!is.null(DF)) {
    if (!(registry %in% DBI::dbListTables(con))) {
      DBI::dbCreateTable(con, registry, DF)
      DBI::dbAppendTable(con, registry, DF)
    } else if (nrow(DBI::dbReadTable(con, registry) |> dplyr::filter(Query_Date == Sys.Date())) == 0) {
      DBI::dbAppendTable(con, registry, DF)
    } else if (nrow(DBI::dbReadTable(con, registry) |> dplyr::filter(Query_Date == Sys.Date())) > 0) {
      temp_df <- DBI::dbReadTable(con, registry) |> dplyr::filter(Query_Date < Sys.Date())
      DBI::dbWriteTable(con, registry, temp_df, overwrite = TRUE)
      DBI::dbAppendTable(con, registry, DF)
    }
  }
}

# API call generating functions
# Generate NCT URLS
generate_NCT_URL <- function(NCT_ID_Vec) {
  api_url <-
    paste0(
      "https://clinicaltrials.gov/api/v2/studies?format=json&filter.ids=",
      NCT_ID_Vec,
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

# Return NCT Dataframe
# verbose = true because for some reason this fails on linux otherwise....
generate_NCT_DF <- function(url) {
  df <- url |>
    httr::GET(verbose = TRUE) |>
    httr::content() |>
    tibble::tibble() |>
    head(1) |>
    dplyr::rename(studies = 1) |>
    tidyr::unnest_longer(studies) |>
    tidyr::unnest_wider(studies) |>
    tidyr::unnest_wider(protocolSection) |>
    tidyr::unnest_wider(c(identificationModule, statusModule, conditionsModule)) |>
    tidyr::unnest_wider(orgStudyIdInfo) |>
    tidyr::hoist(primaryCompletionDateStruct,
      PrimaryCompletionDate = "date"
    ) |>
    tidyr::hoist(completionDateStruct,
      CompletionDate = "date"
    ) |>
    tidyr::hoist(resultsFirstPostDateStruct,
      ResultsFirstPostDate = "date"
    ) |>
    tidyr::hoist(lastUpdatePostDateStruct,
      LastUpdatePostDate = "date"
    ) |>
    tidyr::hoist(referencesModule,
      SeeAlsoLinkURL = "url"
    ) |>
    tidyr::unnest_wider(referencesModule) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      Condition = stringr::str_c(unlist(conditions),
        collapse = "|"
      ),
      SeeAlsoLinkURL = stringr::str_c(unlist(seeAlsoLinks),
        collapse = "|"
      ),
      OverallStatus = stringr::str_to_sentence(stringr::str_replace_all(overallStatus, "_", " ")),
      OverallStatus = stringr::str_replace(OverallStatus, "Active not recruiting", "Active, not recruiting"),
      OverallStatus = stringr::str_replace(OverallStatus, "Unknown", "Unknown status"),
      .keep = "unused"
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(desc(nctId)) |>
    dplyr::mutate(Rank = dplyr::row_number(), .before = 1) |>
    janitor::clean_names("big_camel") |>
    dplyr::rename(
      NCTId = NctId,
      OrgStudyId = Id,
      SeeAlsoLinkURL = SeeAlsoLinkUrl
    ) |>
    dplyr::select(Rank, NCTId, OrgStudyId, Condition, BriefTitle, Acronym, OverallStatus, everything()) |>
    dplyr::relocate(LastUpdatePostDate, .before = SeeAlsoLinkURL)

  return(df)
}

# Generate ISRCTN Dataframe
generate_ISRCTN_df <- function(ISRCTN_URL) {
  ISRCTN_XML <- httr::GET(ISRCTN_URL) |> httr::content(encoding = "UTF-8")

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
    "Results_date_first_publication" =
      xml2::xml_text(
        xml2::xml_find_all(ISRCTN_XML, ".//results_date_first_publication")
      )
  )
}
# Function to update pubmed tables for one registry
generate_pubmed_results_from_search_terms_and_update_db_one_registry <- function(registry,
                                                                                 trial_id_df = Trial_IDs,
                                                                                 con = DBI::dbConnect(RSQLite::SQLite(), "RSQLite_Data/TrialTracker-db.sqlite"),
                                                                                 mindate = Sys.Date() - 1,
                                                                                 maxdate = Sys.Date() - 1) {
  api <- readr::read_file("Secrets/entrez.key")

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
      dplyr::select(-all_of(registry_id_cols_to_remove)) |>
      dplyr::distinct()

    update_db(con, registry_db_pm_name, pm_tibble)
  }
}

# Wrapper function to update all pubmed tables
update_all_pubmed_tables <- function(registries = c("NCT", "ISRCTN", "NIHR", "EU"), trial_id_df = Trial_IDs) {
  purrr::walk(registries, generate_pubmed_results_from_search_terms_and_update_db_one_registry, trial_id_df = Trial_IDs, con = con)
}

# Function to update db for NCT registry changes
update_db_for_NCT_changes <- function(main_con = DBI::dbConnect(RSQLite::SQLite(), "RSQLite_Data/TrialTracker-db.sqlite"),
                                      Trial_IDs = DBI::dbReadTable(con, "Trial_IDs")) {
  # Collapse Trial ID numbers into search term
  half_NCT_Id_Vector <- round(length(concat_ids(Trial_IDs, "NCT_Ids")) / 2, 0)
  NCT_Id_Vector1 <- concat_ids(Trial_IDs, "NCT_Ids")[1:half_NCT_Id_Vector] |> paste0(collapse = "%7C")
  NCT_Id_Vector2 <- concat_ids(Trial_IDs, "NCT_Ids")[(half_NCT_Id_Vector + 1):length(concat_ids(Trial_IDs, "NCT_Ids"))] |> paste0(collapse = "%7C")

  # Construct URLs

  # The clinicalTrial.gov API can handle 1000 request at a time. Above 1000 and the extract will need to be split.
  # Split URL into 2 as URL too long now

  NCT_URL1 <- generate_NCT_URL(NCT_Id_Vector1)
  NCT_URL2 <- generate_NCT_URL(NCT_Id_Vector2)

  # Results DFs
  NCT_DF1 <- generate_NCT_DF(NCT_URL1)
  NCT_DF2 <- generate_NCT_DF(NCT_URL2)

  NCT_DF <- dplyr::bind_rows(NCT_DF1, NCT_DF2) |>
    dplyr::right_join(Trial_IDs[, c("Program", "Guideline.number", "URL", "NCT_Ids")],
      by = c("NCTId" = "NCT_Ids"), multiple = "all"
    ) |>
    dplyr::filter(!is.na(NCTId)) |>
    dplyr::mutate(Query_Date = Sys.Date()) |>
    dplyr::select(Query_Date, Program, Guideline.number, URL, everything(), -Rank)

  update_db(con, "NCT", NCT_DF)
}

# Function to update db for ISRCTN registry changes
update_db_for_ISRCTN_changes <- function(main_con = DBI::dbConnect(RSQLite::SQLite(), "RSQLite_Data/TrialTracker-db.sqlite"),
                                         Trial_IDs = DBI::dbReadTable(con, "Trial_IDs")) {
  # Collapse trial ids into search terms
  half_ISRCTN_Id_Vector <- round(length(concat_ids(Trial_IDs, "ISRCTN_Ids")) / 2, 0)
  ISRCTN_Id_Vector1 <- concat_ids(Trial_IDs, "ISRCTN_Ids")[1:half_ISRCTN_Id_Vector] |> paste0(collapse = "%20OR%20")
  ISRCTN_Id_Vector2 <- concat_ids(Trial_IDs, "ISRCTN_Ids")[(half_ISRCTN_Id_Vector + 1):length(concat_ids(Trial_IDs, "ISRCTN_Ids"))] |> paste0(collapse = "%20OR%20")

  # ISRCTN URLs done in two parts as v large
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

  # Create intermediate dfs
  ISRCTN_DF1 <- generate_ISRCTN_df(ISRCTN_URL1)
  ISRCTN_DF2 <- generate_ISRCTN_df(ISRCTN_URL2)

  # Create final df
  ISRCTN_DF <- dplyr::bind_rows(ISRCTN_DF1, ISRCTN_DF2) |>
    dplyr::right_join(Trial_IDs[, c("Program", "Guideline.number", "ISRCTN_Ids")], by = c("ISRCTN_No" = "ISRCTN_Ids"), multiple = "all") |>
    dplyr::filter(!is.na(ISRCTN_No)) |>
    dplyr::mutate(Query_Date = Sys.Date()) |>
    dplyr::select(Query_Date, Program, Guideline.number, URL, everything())

  # Update db
  update_db(con, "ISRCTN", ISRCTN_DF)
}

# Function to update db for NIHR registry changes
update_db_for_NIHR_changes <- function(main_con = DBI::dbConnect(RSQLite::SQLite(), "RSQLite_Data/TrialTracker-db.sqlite"),
                                       Trial_IDs = DBI::dbReadTable(con, "Trial_IDs")) {
  # Construct trial id vectors
  NIHR_Id_Vector <- collapse_ids(Trial_IDs, "NIHR_Ids", "+OR+")

  # Construct URL
  NIHR_URL_API2 <-
    "https://nihr.opendatasoft.com/api/v2/catalog/datasets/infonihr-open-dataset/exports/json?limit=-1&offset=0&lang=en&timezone=UTC"

  NIHR_json <- jsonlite::fromJSON(url(NIHR_URL_API2))

  NIHR_Trial_IDs <- Trial_IDs |>
    dplyr::select(Program, Guideline.number, URL, NIHR_Ids) |>
    tidyr::drop_na(NIHR_Ids) |>
    dplyr::mutate("projectjoin" = stringr::str_replace_all(NIHR_Ids, "[^\\d]", ""))

  NIHR_DF <- NIHR_json |>
    dplyr::mutate("projectjoin" = stringr::str_replace_all(project_id, "[^\\d]", "")) |>
    dplyr::right_join(NIHR_Trial_IDs, by = c("projectjoin"), multiple = "all") |>
    tidyr::drop_na(projectjoin) |>
    dplyr::mutate(Query_Date = Sys.Date()) |>
    dplyr::select(Query_Date, Program, Guideline.number, URL, project_id, project_title, project_status, project_id, end_date)

  update_db(main_con, "NIHR", NIHR_DF)
}

# Function to update db for EU registry changes
update_db_for_EU_changes <- function(main_con = DBI::dbConnect(RSQLite::SQLite(), "RSQLite_Data/TrialTracker-db.sqlite"),
                                     Trial_IDs = DBI::dbReadTable(con, "Trial_IDs")) {
  # set permissions for clinicaltrials.eu to work
  httr::set_config(httr::config(ssl_verifypeer = FALSE))

  # Create vector of EU Ids
  EU_Vector <- collapse_ids(Trial_IDs, "EU_Ids", "+OR+")

  # Generate API URL
  EU_URL <- paste0(
    "https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
    EU_Vector
  )

  # sqlite db
  eu_temp_db <- nodbi::src_sqlite(dbname = "RSQLite_Data/EU_temp_db.sqlite", collection = "EU")

  try(ctrdata::ctrLoadQueryIntoDb(queryterm = EU_URL, con = eu_temp_db))

  # Collapse fields into 1 vector
  EU_DF <-
    ctrdata::dbGetFieldsIntoDf(
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
    dplyr::right_join(Trial_IDs, multiple = "all") |>
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
# Wrapper function for all update (no PM or Email)
download_trial_info_wrapper_no_pm_or_email <- function(con = DBI::dbConnect(RSQLite::SQLite(), "RSQLite_Data/TrialTracker-db.sqlite"),
                                                       Trial_IDs = DBI::dbReadTable(con, "Trial_IDs")) {
  # Update dbs
  update_db_for_NCT_changes()
  update_db_for_ISRCTN_changes()
  update_db_for_NIHR_changes()
  update_db_for_EU_changes()
}

# Wrapper function for all update
download_trial_info_wrapper <- function(con = DBI::dbConnect(RSQLite::SQLite(), "RSQLite_Data/TrialTracker-db.sqlite"),
                                        Trial_IDs = DBI::dbReadTable(con, "Trial_IDs"),
                                        dev_flag) {
  download_trial_info_wrapper_no_pm_or_email()
  update_all_pubmed_tables()
  generate_email_alerts(dev_flag = dev_flag)
}
