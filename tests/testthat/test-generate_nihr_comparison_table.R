testthat::test_that("generate_nihr_comparison_table writes NIHR changes with correct args", {
  testthat::skip_if_not_installed("compareDF")
  testthat::skip_if_not_installed("mockery")

  # Minimal but realistic NIHR frames
  new_df <- data.frame(
    Guideline.number = c("GL123", "GL456"),
    project_id = c(1, 2),
    Query_Date = as.Date(c("2023-01-01", "2023-01-02")),
    URL = c("url1", "url2"),
    project_title = c("title1", "title2"),
    stringsAsFactors = FALSE
  )

  old_df <- data.frame(
    Guideline.number = c("GL123", "GL456"),
    project_id = c(1, 2),
    Query_Date = as.Date(c("2022-12-01", "2022-12-02")),
    URL = c("url1", "url2"),
    project_title = c("title1", "title2"),
    stringsAsFactors = FALSE
  )

  # Capture what gets written
  captured <- new.env(parent = emptyenv())
  captured$called <- FALSE
  captured$args <- NULL

  # Stub pull_nihr_change (this is called unqualified, so stub works)
  mockery::stub(
    generate_nihr_comparison_table,
    "pull_nihr_change",
    function(prog_regex, old_or_new, main_con) {
      if (identical(old_or_new, "new")) new_df else old_df
    }
  )

  # Stub write_changes_to_disk (also called unqualified)
  mockery::stub(
    generate_nihr_comparison_table,
    "write_changes_to_disk",
    function(comparison, daily_path, DF_Name, prog_name) {
      captured$called <- TRUE
      captured$args <- list(
        comparison = comparison,
        daily_path = daily_path,
        DF_Name = DF_Name,
        prog_name = prog_name
      )
      TRUE
    }
  )

  out <- generate_nihr_comparison_table(
    program = "Test Program",
    program_regex = "GL.*",
    main_con = NULL,
    daily_path = "mock/path"
  )

  testthat::expect_true(isTRUE(out))
  testthat::expect_true(isTRUE(captured$called))

  testthat::expect_identical(captured$args$daily_path, "mock/path")
  testthat::expect_identical(captured$args$DF_Name, "NIHR")
  testthat::expect_identical(captured$args$prog_name, "Test Program")

  # Sanity check: something got passed through as comparison output
  testthat::expect_false(is.null(captured$args$comparison))
})

testthat::test_that("generate_nihr_comparison_table handles empty inputs", {
  testthat::skip_if_not_installed("compareDF")
  testthat::skip_if_not_installed("mockery")

  # IMPORTANT: empty frames should still have the required columns
  empty_df <- data.frame(
    Guideline.number = character(),
    project_id = integer(),
    Query_Date = as.Date(character()),
    URL = character(),
    project_title = character(),
    stringsAsFactors = FALSE
  )

  captured <- new.env(parent = emptyenv())
  captured$called <- FALSE
  captured$args <- NULL

  mockery::stub(
    generate_nihr_comparison_table,
    "pull_nihr_change",
    function(...) empty_df
  )

  mockery::stub(
    generate_nihr_comparison_table,
    "write_changes_to_disk",
    function(comparison, daily_path, DF_Name, prog_name) {
      captured$called <- TRUE
      captured$args <- list(
        comparison = comparison,
        daily_path = daily_path,
        DF_Name = DF_Name,
        prog_name = prog_name
      )
      TRUE
    }
  )

  out <- generate_nihr_comparison_table(
    program = "Test Program",
    program_regex = "GL.*",
    main_con = NULL,
    daily_path = "mock/path"
  )

  testthat::expect_true(isTRUE(out))
  testthat::expect_true(isTRUE(captured$called))
  testthat::expect_identical(captured$args$DF_Name, "NIHR")
})
