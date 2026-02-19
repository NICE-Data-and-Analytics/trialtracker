test_that("safe_db_getquery returns Error data.frame when con is NULL", {
  # If log_msg isn't part of the package namespace (e.g. still in the Rmd),
  # define a no-op version in the test environment so the function can run.
  if (!exists("log_msg", mode = "function", inherits = TRUE)) {
    log_msg <- function(...) invisible(NULL)
  }

  out <- safe_db_getquery(con = NULL, sql = "SELECT 1", label = "unit")

  expect_s3_class(out, "data.frame")
  expect_named(out, "Error")
  expect_equal(out$Error[[1]], "unit: no DB connection")
})

test_that("safe_db_getquery returns query results on success", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  if (!exists("log_msg", mode = "function", inherits = TRUE)) {
    log_msg <- function(...) stop("log_msg should not be called on success")
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "CREATE TABLE t (x INTEGER, y TEXT)")
  DBI::dbExecute(con, "INSERT INTO t (x, y) VALUES (1, 'a'), (2, 'b')")

  out <- safe_db_getquery(con, "SELECT * FROM t ORDER BY x", label = "unit")

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
  expect_equal(out$x, c(1L, 2L))
  expect_equal(out$y, c("a", "b"))
})

test_that("safe_db_getquery returns Error data.frame on SQL error", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  # Capture logs if log_msg exists / gets defined locally
  logs <- character(0)
  log_msg <- function(...) {
    logs <<- c(logs, paste(..., collapse = " "))
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  out <- safe_db_getquery(con, "SELECT * FROM does_not_exist", label = "unit")

  expect_s3_class(out, "data.frame")
  expect_named(out, "Error")
  expect_match(out$Error[[1]], "^unit: ", perl = TRUE)

  # If logging is wired, we should have at least one entry
  expect_true(length(logs) >= 1L)
  expect_match(logs[[1]], "DB ERROR:", fixed = TRUE)
})

test_that("safe_db_getquery accepts DBI::SQL input", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  if (!exists("log_msg", mode = "function", inherits = TRUE)) {
    log_msg <- function(...) stop("log_msg should not be called on success")
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "CREATE TABLE t (x INTEGER)")
  DBI::dbExecute(con, "INSERT INTO t (x) VALUES (1), (2)")

  out <- safe_db_getquery(
    con,
    DBI::SQL("SELECT x FROM t ORDER BY x"),
    label = "unit"
  )

  expect_equal(out$x, c(1L, 2L))
})
