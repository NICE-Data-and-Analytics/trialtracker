# Auto-converted scaffold from index.qmd
library(shiny)
library(bslib)

ui <- navbarPage(
  title = "TrialTracker Dashboard",
  theme = bs_theme(),
  header = tagList(
      tags$head(includeHTML("Secrets/GA_Script.html")),
      tags$head(tags$link(rel="stylesheet", type="text/css", href="styles.css")),
  ),
  tabPanel("Home",
    fluidPage(
      h2("TrialTracker Dashboard"),
      p("This is an initial port from Quarto. Replace this with your actual UI components."),
      tabsetPanel(
      tabPanel("ClinicalTrials.gov", div(class="pt-3", p("TODO: add UI for ClinicalTrials.gov")))
      tabPanel("ISRCTN", div(class="pt-3", p("TODO: add UI for ISRCTN")))
      tabPanel("NIHR", div(class="pt-3", p("TODO: add UI for NIHR")))
      tabPanel("Clinicaltrials.eu", div(class="pt-3", p("TODO: add UI for Clinicaltrials.eu")))
      tabPanel("Add Trial", div(class="pt-3", p("TODO: add UI for Add Trial")))
      tabPanel("Add Multiple Trials", div(class="pt-3", p("TODO: add UI for Add Multiple Trials")))
      tabPanel("Remove or Archive Trial", div(class="pt-3", p("TODO: add UI for Remove or Archive Trial")))
      tabPanel("Trial Comments", div(class="pt-3", p("TODO: add UI for Trial Comments")))
      tabPanel("ClinicalTrials.gov Archive", div(class="pt-3", p("TODO: add UI for ClinicalTrials.gov Archive")))
      tabPanel("ISRCTN Archive", div(class="pt-3", p("TODO: add UI for ISRCTN Archive")))
      tabPanel("NIHR Archive", div(class="pt-3", p("TODO: add UI for NIHR Archive")))
      tabPanel("EU Archive", div(class="pt-3", p("TODO: add UI for EU Archive")))
      )
    )
  )
)

server <- function(input, output, session) {
  # ---- Server logic ported from `index.qmd` ----
  #| echo: false
  #| include: false

  main_con <- DBI::dbConnect(RSQLite::SQLite(), file.path("inst", "extdata", "RSQLite_data", "TrialTracker-db.sqlite"))
  trial_id_df <- DBI::dbReadTable(main_con, "Trial_Ids")

  # Regex patterns
  NCT_pattern <- "NCT[0-9]{8}"
  EU_pattern <- "[0-9]{4}-[0-9]{6}-[0-9]{2}"
  ISRCTN_pattern <- "ISRCTN[0-9]{8}"
  NIHR_pattern <- "(NIHR[0-9]{0,6})|(RP-PG-[0-9]{4}-[0-9]{4,5})|([0-9]{2,3}[/-][0-9]{2,4}[/-][0-9]{2,4})|(^ICA-.*-[0-9]{3})"

  # Helper functions (non-reactive and used for rendering UI)

  # Clean UTF function
  clean_utf8 <- function(x){
    iconv(x, "UTF-8", "UTF-8", sub = "")
  }

  # Render pubmed
  render_pubmed_table <- function(table_name) {
    if (!table_name %in% DBI::dbListTables(main_con)) return(NULL)

    DBI::dbReadTable(main_con, table_name) |>
      dplyr::mutate(across(everything(), .fns = clean_utf8)) |>
      dplyr::mutate(
        Query_Date = format(as.Date(as.numeric(Query_Date), origin = "1970-01-01"), "%Y/%m/%d"),
        abstract = stringr::str_trunc(abstract, 200L)
      ) |>
      dplyr::rename(
        `Query Date` = Query_Date,
        `Trial ID` = ID,
        `Publication DOI` = doi,
        Title = title,
        Abstract = abstract,
        Journal = jabbrv,
        Condition = `Short..working.title.`,
        Guideline = Guideline.number
      ) |>
      dplyr::select(
        Program, Guideline, `Query Date`, `Trial ID`, pmid,
        `Publication DOI`, Title, Abstract, Journal, Condition
      ) |>
      dplyr::distinct() |>
      dplyr::arrange(dplyr::desc(`Query Date`)) |>
      DT::datatable(options = list(bPaginate = FALSE, scrollY = TRUE), filter = "top")
  }

  # Inititalise reactive vals for live trial info tables
  nct_data <- shiny::reactiveVal(DBI::dbReadTable(main_con, "NCT"))
  isrctn_data <- shiny::reactiveVal(DBI::dbReadTable(main_con, "ISRCTN"))
  eu_data <- shiny::reactiveVal(DBI::dbReadTable(main_con, "EU"))
  nihr_data <- shiny::reactiveVal(DBI::dbReadTable(main_con, "NIHR"))

  # Initialise timestamp 
  last_refreshed <- shiny::reactiveVal(file.info("inst/extdata/RSQLite_data/TrialTracker-db.sqlite")$mtime)

  # Create output for 'last_updated' timestamp
  output$timestamp_text_live <- shiny::renderText({
    paste(
      "🔄 Last refreshed at:",
      format(lubridate::with_tz(last_refreshed(), "UTC"), "%Y-%m-%d %H:%M:%S UTC")
    )
  })
  output$timestamp_text_recents <- shiny::renderText({
    paste(
      "🔄 Last refreshed at:",
      format(lubridate::with_tz(last_refreshed(), "UTC"), "%Y-%m-%d %H:%M:%S UTC")
    )
  })
  output$timestamp_text_no_changes <- shiny::renderText({
    paste(
      "🔄 Last refreshed at:",
      format(lubridate::with_tz(last_refreshed(), "UTC"), "%Y-%m-%d %H:%M:%S UTC")
    )
  })


  # Central refresh function
  refresh_all_data <- function() {
  
    #Call Update Script
    trialtracker::download_trial_info_wrapper_no_pm_or_email()

    #Update reactives with new sqlite tables
    nct_data(DBI::dbReadTable(main_con, "NCT"))
    isrctn_data(DBI::dbReadTable(main_con, "ISRCTN"))
    eu_data(DBI::dbReadTable(main_con, "EU"))
    nihr_data(DBI::dbReadTable(main_con, "NIHR"))

    last_refreshed(file.info("inst/extdata/RSQLite_data/TrialTracker-db.sqlite")$mtime)
  }

  # Shared helper for rendering livetrialinfos
  render_live_trial_table <- function(data, id_col, ids, rename_list, reorder_cols = NULL, extra_mutate = NULL, extra_filter = NULL) {
    data_filtered <- if (is.function(extra_filter)) extra_filter(data) else data

    date_added_df <- data_filtered |>
      dplyr::summarise(`Date Added` = lubridate::as_date(min(Query_Date - 1L)), .by = {{ id_col }})

    processed <- data_filtered |>
      dplyr::filter({{ id_col }} %in% ids) |>
      dplyr::left_join(date_added_df, by = rlang::as_string(rlang::ensym(id_col))) |>
      dplyr::slice_max(Query_Date, n = 1L, with_ties = TRUE) |>
      dplyr::select(-Query_Date) |>
      dplyr::mutate(across(.cols = everything(), .fns = clean_utf8))

    if (is.function(extra_mutate)) processed <- extra_mutate(processed)

    processed <- processed |>
      dplyr::rename(!!!rename_list)

    if (!is.null(reorder_cols)) {
      processed <- processed |>
        dplyr::select(dplyr::all_of(reorder_cols))
    }

    processed |>
      dplyr::arrange(desc(`Date Added`)) |>
      dplyr::distinct()
  }

  # Live trial status tables
  output$live_trial_table_nct <- DT::renderDT({
  
    req(main_con, trial_id_df, nct_data())

    render_live_trial_table(
      data = nct_data(),
      id_col = NCTId,
      ids = trial_id_df$NCT_Ids,
      rename_list = c(
        Guideline = "Guideline.number",
        `NCT ID` = "NCTId",
        `Org Study ID` = "OrgStudyId",
        Title = "BriefTitle",
        Status = "OverallStatus",
        `Primary Completion Date` = "PrimaryCompletionDate",
        `Completion Date` = "CompletionDate",
        `Results Submit Date` = "ResultsFirstSubmitDate",
        `Results Post Date` = "ResultsFirstPostDate",
        `Last Update` = "LastUpdatePostDate"
      ),
      reorder_cols = c("Program", "Guideline", "Date Added", "NCT ID", "Org Study ID", "Title", "Status",
                       "Primary Completion Date", "Completion Date", "Results Submit Date", "Results Post Date", "Last Update"),
      extra_mutate = ~ dplyr::mutate(., across(PrimaryCompletionDate:LastUpdatePostDate,
                                  ~ lubridate::as_date(lubridate::parse_date_time(., c("ymd", "bdy"), truncated = 1))))
    )
  }, options = list(bPaginate = FALSE, scrollY = TRUE), filter = "top")
  output$live_trial_table_isrctn <- DT::renderDT({
  
    req(main_con, trial_id_df, isrctn_data())

    render_live_trial_table(
      data = isrctn_data(),
      id_col = ISRCTN_No,
      ids = trial_id_df$ISRCTN_Ids,
      rename_list = c(
        Guideline = "Guideline.number",
        `ISRCTN ID` = "ISRCTN_No",
        Title = "Public_Title",
        `Scientific Title` = "Scientific_Title",
        `Recruitment Status` = "Recruitment_Status",
        `Results URL` = "Results_url_link",
        `Results summary` = "Results_summary"
      ),
      reorder_cols = c("Program", "Guideline", "Date Added", "URL", "ISRCTN ID", "Title", "Acronym",
                       "Scientific Title", "Recruitment Status", "Results URL", "Results summary",
                       "Results_date_completed", "Results_date_posted", "Results_date_first_publication", "Comments"),
      extra_mutate = ~ dplyr::mutate(.,
        `Results_date_completed` = lubridate::dmy(Results_date_completed),
        `Results_date_posted` = lubridate::dmy(Results_date_posted),
        `Results_date_first_publication` = lubridate::dmy(Results_date_first_publication)
      )
    )
  }, options = list(bPaginate = FALSE, scrollY = TRUE), filter = "top")
  output$live_trial_table_nihr <- DT::renderDT({
  
    req(main_con, trial_id_df, nihr_data())

    render_live_trial_table(
      data = nihr_data(),
      id_col = project_id,
      ids = trial_id_df$NIHR_Ids,
      rename_list = c(
        Guideline = "Guideline.number",
        `NIHR ID` = "project_id",
        `Project Title` = "project_title",
        Status = "project_status",
        `End Date` = "end_date"
      ),
      reorder_cols = c("Program", "Guideline", "Date Added", "NIHR ID", "Project Title", "Status", "End Date", "Comments"),
      extra_filter = ~ dplyr::mutate(., project_id_filter = stringr::str_replace_all(project_id, "[^\\d]", "")) |>
                     dplyr::filter(project_id_filter %in% stringr::str_replace_all(trial_id_df$NIHR_Ids, "[^\\d]", "")) |>
                     dplyr::select(-project_id_filter)
    )
  }, options = list(bPaginate = FALSE, scrollY = TRUE), filter = "top")
  output$live_trial_table_eu <- DT::renderDT({
  
    req(main_con, trial_id_df, eu_data())

    render_live_trial_table(
      data = eu_data(),
      id_col = EU_Ids,
      ids = trial_id_df$EU_Ids,
      rename_list = c(
        Guideline = "Guideline.number",
        `Clinicaltrials.eu ID` = "X_id",
        `End of Trial Status` = "p_end_of_trial_status",
        Title = "a3_full_title_of_the_trial",
        `Abbreviated Title` = "a32_name_or_abbreviated_title_of_the_trial_where_available",
        `Sponsor ID` = "a41_sponsors_protocol_code_number"
      ),
      reorder_cols = c("Program", "Guideline", "Date Added", "Clinicaltrials.eu ID", "Title",
                       "Abbreviated Title", "End of Trial Status", "Sponsor ID", "Comments"),
      extra_filter = ~ dplyr::mutate(., short_id = stringr::str_sub(X_id, 1L, 14L)) |>
                     dplyr::filter(short_id %in% trial_id_df$EU_Ids) |>
                     dplyr::select(-short_id, -a31_title_of_the_trial_for_lay_people_in_easily_understood_ie_nontechnical_language, -EU_Ids)
    )
  }, options = list(bPaginate = FALSE, scrollY = TRUE), filter = "top")

  # Variable for cutoff date (31 days)
  cutoff_date <- Sys.Date() - 31L

  # Helper function to generate old and new dfs for changes
  get_new_old_df <- function(data, id_col, ids) {
    new_df <- data |>
      dplyr::filter({{ id_col }} %in% ids) |>
      dplyr::slice_max(Query_Date, n = 1L, with_ties = TRUE)

    old_df <- data |>
      dplyr::filter(Query_Date <= cutoff_date, {{ id_col }} %in% ids) |>
      dplyr::slice_max(Query_Date, n = 1L, with_ties = TRUE)

    list(new = new_df, old = old_df)
  }

  # Recent Status Changes Tables
  output$nct_monthly_change <- DT::renderDT({
  
    req(main_con, trial_id_df, nct_data())

    dfs <- get_new_old_df(nct_data(), NCTId, trial_id_df$NCT_Ids)

    compareDF::compare_df(
      dfs$new,
      dfs$old,
      group_col = c("Guideline.number", "NCTId"),
      exclude = c("Query_Date", "Comments", "Condition", "BriefTitle", "Acronym", "PrimaryCompletionDate", "ResultsFirstSubmitDate", "SeeAlsoLinkURL"),
      stop_on_error = FALSE
    )$comparison_df |>
      dplyr::left_join(dfs$new |> dplyr::select(BriefTitle, Program, Guideline.number, URL, NCTId, Condition, ResultsFirstSubmitDate, Acronym, Comments),
                       by = c("Program", "Guideline.number", "URL", "NCTId")) |>
      dplyr::mutate(
        URL2 = paste0("https://clinicaltrials.gov/ct2/show/", NCTId),
        Title = stringr::str_trunc(BriefTitle, 80L),
        Condition = stringr::str_trunc(Condition, 30L)
      ) |>
      dplyr::mutate(across(everything(), .fns = clean_utf8)) |>
      dplyr::mutate(across(c(CompletionDate:LastUpdatePostDate),
                           ~ lubridate::as_date(lubridate::parse_date_time(.x, c("ymd", "bdy"), truncated = 1L)))) |>
      dplyr::select(Change = chng_type, Program, Guideline = Guideline.number, URL = URL2, Acronym, Title, Condition,
                    Status = OverallStatus, `Completion Date` = CompletionDate,
                    `Results Submit Date` = ResultsFirstSubmitDate,
                    `Last Update Date` = LastUpdatePostDate, Comments) |>
      dplyr::distinct()
  }, options = list(bPaginate = FALSE, scrollY = TRUE), filter = "top")
  output$isrctn_monthly_change <- DT::renderDT({
  
    req(main_con, trial_id_df, isrctn_data())

    dfs <- get_new_old_df(isrctn_data(), ISRCTN_No, trial_id_df$ISRCTN_Ids)

    compareDF::compare_df(
      dfs$new,
      dfs$old,
      group_col = c("Guideline.number", "ISRCTN_No"),
      exclude = c("Query_Date", "Comments"),
      stop_on_error = FALSE
    )$comparison_df |>
      dplyr::left_join(dfs$new) |>
      dplyr::mutate(
        Title = stringr::str_trunc(Scientific_Title, 75L),
        across(everything(), .fns = clean_utf8),
        Results_date_completed = lubridate::dmy(Results_date_completed),
        Results_date_posted = lubridate::dmy(Results_date_posted),
        Results_date_first_publication = lubridate::dmy(Results_date_first_publication)
      ) |>
      dplyr::select(Change = chng_type, Program, Guideline = Guideline.number, URL, Acronym, Title,
                    Status = Recruitment_Status, `Results URL` = Results_url_link, `Results Summary` = Results_summary,
                    `Results Completed` = Results_date_completed, `Results Posted` = Results_date_posted,
                    `Results Published` = Results_date_first_publication) |>
      dplyr::distinct()
  }, options = list(bPaginate = FALSE, scrollY = TRUE), filter = "top")
  output$nihr_monthly_change <- DT::renderDT({
  
    req(main_con, trial_id_df, nihr_data())

    trial_ids <- stringr::str_replace_all(trial_id_df$NIHR_Ids, "[^\\d]", "")

    dfs <- get_new_old_df(
      nihr_data() |> dplyr::mutate(project_id_filter = stringr::str_replace_all(project_id, "[^\\d]", "")),
      project_id_filter,
      trial_ids
    )

    dfs <- lapply(dfs, function(df) df |> dplyr::select(-project_id_filter))

    compareDF::compare_df(
      dfs$new,
      dfs$old,
      group_col = c("Guideline.number", "project_id"),
      exclude = c("Query_Date", "Comments"),
      stop_on_error = FALSE
    )$comparison_df |>
      dplyr::left_join(dfs$new) |>
      dplyr::mutate(
        Title = stringr::str_trunc(project_title, 90L),
        across(everything(), .fns = clean_utf8)
      ) |>
      dplyr::select(
        Change = chng_type,
        Program,
        Guideline = Guideline.number,
        URL,
        `NIHR ID` = project_id,
        Title,
        Status = project_status,
        `End Date` = end_date
      ) |>
      dplyr::distinct()
  }, options = list(bPaginate = FALSE, scrollY = TRUE), filter = "top")
  output$clinicaltrialseu_monthly_change <- DT::renderDT({
  
    req(main_con, trial_id_df, eu_data())

    dfs <- get_new_old_df(eu_data(), EU_Ids, trial_id_df$EU_Ids)

    compareDF::compare_df(
      dfs$new,
      dfs$old,
      group_col = c("Guideline.number", "EU_Ids"),
      exclude = c("Query_Date", "Comments"),
      stop_on_error = FALSE
    )$comparison_df |>
      dplyr::left_join(dfs$new) |>
      dplyr::mutate(
        Title = stringr::str_trunc(a3_full_title_of_the_trial, 90L),
        across(everything(), .fns = clean_utf8)
      ) |>
      dplyr::select(
        Change = chng_type,
        Program,
        Guideline = Guideline.number,
        URL,
        `EU ID` = EU_Ids,
        Status = p_end_of_trial_status,
        Acronym = a32_name_or_abbreviated_title_of_the_trial_where_available,
        Title
      ) |>
      dplyr::distinct()
  }, options = list(bPaginate = FALSE, scrollY = TRUE), filter = "top")

  # Shared helper for no-change tables
  generate_nochange_table <- function(data, id_col, rename_list, drop_cols = NULL, reorder_cols = NULL, date_cols = NULL, extra_mutate = NULL) {
    req(data)

    max_date <- max(data$Query_Date, na.rm = TRUE)
    old_date <- max_date - months(6L)

    current <- dplyr::filter(data, Query_Date == max_date)
    old <- dplyr::filter(data, Query_Date == old_date)

    date_added <- data |>
      dplyr::summarise(`Date Added` = lubridate::as_date(min(Query_Date - 1L)), .by = {{ id_col }})

    joined <- dplyr::inner_join(current, old, by = names(current)[2:(ncol(current) - 1)], keep = FALSE) |>
      dplyr::select(-contains(".x"), -contains(".y")) |>
      dplyr::left_join(date_added, by = rlang::as_string(rlang::ensym(id_col))) |>
      dplyr::mutate(across(.cols = everything(), .fns = clean_utf8))

    if (!is.null(date_cols)) {
      joined <- joined |>
        dplyr::mutate(across(all_of(date_cols), ~ lubridate::as_date(lubridate::parse_date_time(.x, c("ymd", "bdy", "dmy"), truncated = 1))))
    }

    if (is.function(extra_mutate)) {
      joined <- extra_mutate(joined)
    }

    if (!is.null(drop_cols)) {
      joined <- dplyr::select(joined, -dplyr::any_of(drop_cols))
    }

    joined <- joined |>
      dplyr::rename(!!!rename_list)

    if (!is.null(reorder_cols)) {
      joined <- dplyr::select(joined, dplyr::all_of(reorder_cols))
    }

    joined |>
      dplyr::distinct()
  }

  # Tables
  output$no_change_nct <- DT::renderDT({
    generate_nochange_table(
      data = nct_data(),
      id_col = NCTId,
      rename_list = c(
        "Guideline" = "Guideline.number",
        `NCT ID` = "NCTId",
        `Org Study ID` = "OrgStudyId",
        Title = "BriefTitle",
        Status = "OverallStatus",
        `Primary Completion Date` = "PrimaryCompletionDate",
        `Completion Date` = "CompletionDate",
        `Results Submit Date` = "ResultsFirstSubmitDate",
        `Results Post Date` = "ResultsFirstPostDate",
        `Last Update` = "LastUpdatePostDate"
      ),
      drop_cols = "SeeAlsoLinkURL",
      reorder_cols = c("Program", "Guideline", "Date Added", "NCT ID", "Org Study ID", "Title", "Status",
                       "Primary Completion Date", "Completion Date", "Results Submit Date", "Results Post Date", "Last Update"),
      date_cols = c("PrimaryCompletionDate", "CompletionDate", "ResultsFirstSubmitDate", "ResultsFirstPostDate", "LastUpdatePostDate")
    )
  }, options = list(bPaginate = FALSE, scrollY = TRUE), filter = "top")
  output$no_change_isrctn <- DT::renderDT({
    generate_nochange_table(
      data = isrctn_data(),
      id_col = ISRCTN_No,
      rename_list = c(
        "Guideline" = "Guideline.number",
        `ISRCTN ID` = "ISRCTN_No",
        Title = "Public_Title",
        `Scientific Title` = "Scientific_Title",
        `Recruitment Status` = "Recruitment_Status",
        `Results completed date` = "Results_date_completed",
        `Results URL` = "Results_url_link",
        `Results summary` = "Results_summary",
        `Results posted date` = "Results_date_posted",
        `Results published date` = "Results_date_first_publication"
      ),
      reorder_cols = c("Program", "Guideline", "Date Added", "ISRCTN ID", "Title", "Scientific Title",
                       "Recruitment Status", "Results URL", "Results summary",
                       "Results completed date", "Results posted date", "Results published date"),
      extra_mutate = ~ dplyr::mutate(.,
        Results_date_completed = lubridate::dmy(Results_date_completed),
        Results_date_posted = lubridate::dmy(Results_date_posted),
        Results_date_first_publication = lubridate::dmy(Results_date_first_publication)
      )
    )
  }, options = list(bPaginate = FALSE, scrollY = TRUE), filter = "top")
  output$no_change_nihr <- DT::renderDT({
    generate_nochange_table(
      data = nihr_data(),
      id_col = project_id,
      rename_list = c(
        "Guideline" = "Guideline.number",
        `NIHR ID` = "project_id",
        `Project Title` = "project_title",
        Status = "project_status",
        `End Date` = "end_date"
      ),
      reorder_cols = c("Program", "Guideline", "Date Added", "NIHR ID", "Project Title", "Status", "End Date")
    )
  }, options = list(bPaginate = FALSE, scrollY = TRUE), filter = "top")
  output$no_change_cteu <- DT::renderDT({
    generate_nochange_table(
      data = eu_data() |> dplyr::select(-a31_title_of_the_trial_for_lay_people_in_easily_understood_ie_nontechnical_language, -EU_Ids),
      id_col = X_id,
      rename_list = c(
        "Guideline" = "Guideline.number",
        `Clinicaltrials.eu ID` = "X_id",
        `End of Trial Status` = "p_end_of_trial_status",
        Title = "a3_full_title_of_the_trial",
        `Abbreviated Title` = "a32_name_or_abbreviated_title_of_the_trial_where_available",
        `Sponsor ID` = "a41_sponsors_protocol_code_number"
      ),
      reorder_cols = c("Program", "Guideline", "Date Added", "Clinicaltrials.eu ID", "Title",
                       "Abbreviated Title", "End of Trial Status", "Sponsor ID")
    )
  }, options = list(bPaginate = FALSE, scrollY = TRUE), filter = "top")

  #### Functionality to add / remove / archive /comment

  ### ADD SINGLE TRIAL
  
  # SQL template for adding single trial
  sql_add <-
    "INSERT INTO Trial_Ids ([Program], [Guideline.number], [URL], [?registry]) VALUES (?program_value, ?Guideline_Reference, ?URL, ?ID)"

  # Reactive to create SQL statement to add single trial
  exp_add <- shiny::reactive({
    DBI::sqlInterpolate(
      main_con,
      sql_add,
      registry = DBI::SQL(input$registry_add),
      program_value = input$guideline_program_add,
      Guideline_Reference = input$guideline_reference_add,
      URL = stringr::str_trim(input$URL_add),
      ID = stringr::str_trim(input$ID_add)
    )
  })

  # Single trial add logic
  shiny::observeEvent(input$button_add, {
    valid_id_check <- stringr::str_detect(input$ID_add, paste(c(NCT_pattern, EU_pattern, ISRCTN_pattern, NIHR_pattern), collapse = "|"))

    if (valid_id_check) {
      shinybusy::show_modal_spinner(spin = "semipolar", color = "#344feb", text = "Adding Trial - please wait while the dashboard refreshes in background....")

      DBI::dbExecute(main_con, exp_add())

      refresh_all_data()

      shinybusy::remove_modal_spinner()

      output$single_result <- shiny::renderUI({
        shiny::HTML(as.character(div(
          style = "color: green;",
          "Trial Added!"
        )))
      })
    } else if (!valid_id_check) {
      output$single_result <- shiny::renderUI({
        shiny::HTML(as.character(div(
          style = "color: red;",
          "Error - Invalid trial ID reference number, please check ID against the above approved formats"
        )))
      })
    }
  })

  ## ADD MULTIPLE TRIALS

  # Function to allow downloading of template
  output$downloadtemplate <- shiny::downloadHandler(
    filename = function() {
      "trialuploadtemplate.csv"
    },
    content = function(file) {
      readr::write_csv(
        readr::read_csv("trialuploadtemplate.csv",
          col_types = readr::cols(.default = readr::col_character())
        ),
        file,
        na = ""
      )
    }
  )

  # Reactive for multiple uploads
  to_add <- shiny::reactive({
    inFile <- input$uploadtemplate
    upload <- readr::read_csv(file = inFile$datapath, col_types = readr::cols(.default = readr::col_character()))
  })

  # Multiple upload logic
  shiny::observeEvent(input$uploadtemplate, {
    NCT_check <- stringr::str_detect(tidyr::replace_na(to_add()$NCT_Ids, ""), NCT_pattern)
    EU_check <- stringr::str_detect(tidyr::replace_na(to_add()$EU_Ids, ""), EU_pattern)
    ISRCTN_check <- stringr::str_detect(tidyr::replace_na(to_add()$ISRCTN_Ids, ""), ISRCTN_pattern)
    NIHR_check <- stringr::str_detect(tidyr::replace_na(to_add()$NIHR_Ids, ""), NIHR_pattern)
    Valid_ID <- {
      NCT_check | EU_check | ISRCTN_check | NIHR_check
    }
  
    errors <- which(!Valid_ID)

    ## Commented out as only used for debugging
    # output$errors <- shiny::renderText({
    #   paste0("errors = ", as.character(errors))
    # })
    # output$errors_length <- shiny::renderText({
    #   paste0("errors_length = ", as.character(length(errors)))
    # })

    if (length(errors) > 0L) {
      output$multi_result <- shiny::renderText({
        shiny::HTML(as.character(div(
          style = "color: red;",
          paste(
            "ERROR - Missing or incorrect Trial ID on line(s)", as.character(errors),
            ", please correct these IDs and reupload the file"
          )
        )))
      })
    } else if (length(errors) == 0L) {
      output$multi_result <- shiny::renderText({
        shiny::HTML(as.character(div(
          style = "color: green;",
          "File has passed input checks and is ready for upload"
        )))
      })
    }
  })
  shiny::observeEvent(input$button_add_multi, {
    NCT_check <- stringr::str_detect(tidyr::replace_na(to_add()$NCT_Ids, ""), NCT_pattern)
    EU_check <- stringr::str_detect(tidyr::replace_na(to_add()$EU_Ids, ""), EU_pattern)
    ISRCTN_check <- stringr::str_detect(tidyr::replace_na(to_add()$ISRCTN_Ids, ""), ISRCTN_pattern)
    NIHR_check <- stringr::str_detect(tidyr::replace_na(to_add()$NIHR_Ids, ""), NIHR_pattern)
    Valid_ID <- {
      NCT_check | EU_check | ISRCTN_check | NIHR_check
    }
    multi_errors <- which(!Valid_ID)

    ## Commented out as only used for debugging
    # output$multi_errors <- shiny::renderText({
    #   paste0("multi_errors = ", as.character(multi_errors))
    # })
    # output$multi_errors_length <- shiny::renderText({
    #   paste0("multi_errors_length = ", as.character(length(multi_errors)))
    # })

    if (length(multi_errors) == 0L) {
      shinybusy::show_modal_spinner(spin = "semipolar", color = "#344feb", text = "Adding Trials - please wait while the dashboard refreshes in background....")

      DBI::dbAppendTable(main_con, "Trial_Ids", to_add())

      refresh_all_data()

      shinybusy::remove_modal_spinner()

      output$multi_result <- shiny::renderText({
        shiny::HTML(as.character(div(
          style = "color: green;",
          "Multiple Trials Added! - please refresh browser to see trial details on Live Trial Info tab"
        )))
      })
    } else if (length(multi_errors) > 0L) {
      output$multi_result <- shiny::renderText({
        shiny::HTML(as.character(div(
          style = "color: red;",
          "Error - Please correct errors in trial ID numbers on upload sheet before resubmitting"
        )))
      })
    }
  })

  #### REMOVE OR ARCHIVE TRIALS

  # SQL template for deleting trial id from Trial_Ids
  sql_delete <- "DELETE FROM Trial_Ids WHERE ?ColID = ?ID_Selected"

  # Reactive to pull id to insert into SQL delete request
  exp_delete <- shiny::reactive({
    DBI::sqlInterpolate(
      main_con,
      sql_delete,
      ColID = DBI::SQL(dplyr::case_when(
        input$registry_remove == "NIHR_Ids" ~ "REPLACE(REPLACE(NIHR_Ids, '-', ''),'/','')",
        TRUE ~ input$registry_remove
      )),
      ID_Selected = if (input$registry_remove == "NIHR_Ids") {
        stringr::str_replace_all(input$ID_remove, "(/)|(-)", "")
      } else {
        input$ID_remove
      }
    )
  })

  # Render output [used in debugging when enabled in UI]
  output$exp_delete <- shiny::renderText(exp_delete())

  #Delete logic
  shiny::observeEvent(input$button_delete, {
    shinybusy::show_modal_spinner(spin = "semipolar", color = "#344feb", text = "Deleting Trial - please wait while the dashboard refreshes in background....")

    DBI::dbExecute(main_con, exp_delete())

    refresh_all_data()

    shinybusy::remove_modal_spinner()

    output$success_delete <- shiny::renderText("Trial Deleted! Refresh browser for this to update on the live trial lists")
  })

  #Update remove list
  shiny::observeEvent(input$registry_remove, {
    choices_list <- DBI::dbReadTable(main_con, "Trial_Ids") |>
      dplyr::select(ids = input$registry_remove) |>
      tidyr::drop_na() |>
      dplyr::arrange(stringr::str_remove_all(ids, "[A-Z]|-")) |> 
      dplyr::pull(ids)
  
    shiny::updateSelectInput(session, "ID_remove", choices = choices_list)
  
    #output$choices_list <- shiny::renderText(choices_list)
  
  })

  # SQL template for SQL archive select statement
  sql_archive <- "SELECT * FROM ?Orig_Table WHERE ?ColID = ?ID_Selected"

  # Reactive for pulling id to insert into SQL archiving request
  exp_archive_pull <- shiny::reactive({
    DBI::sqlInterpolate(
      main_con,
      sql_archive,
      Orig_Table = DBI::SQL(stringr::str_sub(input$registry_remove, end = -5L)),
      ColID = DBI::SQL(dplyr::case_when(
        input$registry_remove == "NCT_Ids" ~ "NCTId",
        input$registry_remove == "NIHR_Ids" ~ "REPLACE(REPLACE(project_id,'-',''),'/','')",
        input$registry_remove == "ISRCTN_Ids" ~ "ISRCTN_No",
        input$registry_remove == "EU_Ids" ~ "EU_Ids"
      )),
      ID_Selected = if (input$registry_remove == "NIHR_Ids") {
        stringr::str_replace_all(input$ID_remove, "(/)|(-)", "")
      } else {
        input$ID_remove
      }
    )
  })

  # Render output [used in debugging when enabled in UI]
  output$exp_archive <- shiny::renderText(exp_archive_pull())

  #Archive logic
  shiny::observeEvent(input$button_archive, {
    shinybusy::show_modal_spinner(spin = "semipolar", color = "#344feb", text = "Archiving Trial - please wait while the dashboard refreshes in background....")

    pulled_trial_data <- DBI::dbGetQuery(main_con, exp_archive_pull()) |>
      dplyr::slice_max(Query_Date) |>
      dplyr::mutate("Archiving_Comments" = stringr::str_remove(paste(Comments, as.character(input$comments_archive), collapse = " "), "NA"))

    # write to db (append)
    DBI::dbWriteTable(main_con, paste0(stringr::str_sub(input$registry_remove, end = -5L), "_rxv"), pulled_trial_data, append = TRUE)

    DBI::dbExecute(main_con, exp_delete())

    refresh_all_data()

    shinybusy::remove_modal_spinner()

    # return some success message
    output$success_archive <- shiny::renderText("Trial Successfully Archived. It should disappear from live lists and appear in archive on browser refresh")
  })

  ### TRIAL COMMENTS

  # Update comment choice list
  shiny::observeEvent(input$registry_comment, {
    choices_list <- DBI::dbReadTable(main_con, "Trial_Ids") |>
      dplyr::select(ids = input$registry_comment) |>
      tidyr::drop_na() |>
      dplyr::arrange(stringr::str_remove_all(ids, "[A-Z]|-")) |> 
      dplyr::pull(ids)

    shiny::updateSelectInput(session, "ID_comment", choices = choices_list)
  })

  # SQL template for pulling comments
  sql_comments_pull <- "SELECT Comments FROM ?Orig_Table WHERE [?ColID] = ?comment_id AND Query_Date IN (SELECT max(Query_Date) FROM ?Orig_Table)"

  # Reactive to generate SQL code based on template
  exp_comment_pull <- shiny::reactive({
    DBI::sqlInterpolate(
      main_con,
      sql_comments_pull,
      Orig_Table = DBI::SQL(stringr::str_sub(input$registry_comment, end = -5L)),
      ColID = DBI::SQL(dplyr::case_when(
        input$registry_comment == "NCT_Ids" ~ "NCTId",
        input$registry_comment == "NIHR_Ids" ~ "project_id",
        input$registry_comment == "ISRCTN_Ids" ~ "ISRCTN_No",
        input$registry_comment == "EU_Ids" ~ "EU_Ids"
      )),
      comment_id = input$ID_comment
    )
  })

  pulled_comments <- shiny::reactive({
    DBI::dbGetQuery(main_con, exp_comment_pull()) |> as.character()
  })

  # Rendering of queries (for debugging if inserted into UI)
  output$comment_sql <- shiny::renderText(exp_comment_pull())
  output$fetch_comment_SQL <- shiny::renderText(pulled_comments())

  # Comment logic
  shiny::observeEvent(input$ID_comment, {
    updateTextAreaInput(session, "trial_comments", value = pulled_comments())
  })

  shiny::observeEvent(input$button_comment, {
    # Add spinner
    shinybusy::show_modal_spinner(spin = "semipolar", color = "#344feb", text = "Amending Comments - please wait while the dashboard refreshes in background....")

    # Take contents of button
    comment_text <- input$trial_comments

    # Derive SQL to insert into table
    comment_insert_sql <- "UPDATE ?TabID SET Comments = ?comment_text WHERE ?ID_name == ?id_no"

    update_comment_sql <-
      DBI::sqlInterpolate(main_con,
        comment_insert_sql,
        TabID = DBI::SQL(stringr::str_sub(input$registry_comment, end = -5L)),
        comment_text = comment_text,
        ID_name = DBI::SQL(dplyr::case_when(
          input$registry_comment == "NCT_Ids" ~ "NCTId",
          input$registry_comment == "NIHR_Ids" ~ "project_id",
          input$registry_comment == "ISRCTN_Ids" ~ "ISRCTN_No",
          input$registry_comment == "EU_Ids" ~ "EU_Ids"
        )),
        id_no = input$ID_comment
      )

    # Action SQL
    DBI::dbExecute(main_con, update_comment_sql)

    # Remove spinner
    shinybusy::remove_modal_spinner()

    # return some success message
    output$success_comment <- shiny::renderText("Comments Successfully Updated - please refresh dashboard to see updated comments")
    })

  # Server code for 'download SQLite databases' button
  
  # Function to allow downloading of files
  output$button_download_dbs <- shiny::downloadHandler(
    filename = function() {
      paste0("databases-", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Create the zip file
      zip::zipr(
        file,
        files = c("Trialtracker-db.sqlite", "EU_temp_db.sqlite"),
        root = "inst\\extdata\\RSQLite_data\\",
        recurse = FALSE,
        include_directories = FALSE
      )
    }
  )
}

shinyApp(ui, server)
