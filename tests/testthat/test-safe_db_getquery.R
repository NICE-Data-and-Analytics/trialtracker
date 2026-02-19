testthat::test_that("safe_db_getquery returns Error df on NULL connection", {
  # keep logging simple but deterministic
  withr::local_options(trialtracker.log_file = tempfile(fileext = ".log"))

  df <- safe_db_getquery(NULL, "SELECT 1", label = "X")
  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_identical(names(df), "Error")
  testthat::expect_match(df$Error[[1]], "X: no DB connection", fixed = TRUE)
})

testthat::test_that("safe_db_getquery returns query result on success", {
  withr::local_options(trialtracker.log_file = tempfile(fileext = ".log"))

  con <- create_sqlite_con()
  df <- safe_db_getquery(con, "SELECT 1 AS a", label = "ok")

  testthat::expect_named(df, "a")
  testthat::expect_equal(df$a[[1]], 1)
})

testthat::test_that("safe_db_getquery returns Error df on SQL error", {
  withr::local_options(trialtracker.log_file = tempfile(fileext = ".log"))

  con <- create_sqlite_con()
  df <- safe_db_getquery(
    con,
    "SELECT * FROM definitely_not_a_table",
    label = "bad"
  )
  testthat::expect_identical(names(df), "Error")
  testthat::expect_match(df$Error[[1]], "^bad: ", perl = TRUE)
})
