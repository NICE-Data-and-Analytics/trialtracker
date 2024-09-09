library(testthat)
library(mockery)
library(dplyr)

source("R/Functions/email_alert_functions.R")

# Mock function to replace dbReadTable
mock_dbReadTable <- function(con, registry) {
  data.frame(Query_Date = as.Date(c("2023-09-01", "2023-09-02", "2023-09-03")))
}

# Test case
test_that("get_last_registry_entry_before_today returns the correct date", {
  # Mock the dbReadTable function
  stub(get_last_registry_entry_before_today, "dbReadTable", mock_dbReadTable)

  # Set the system date to a known value
  withr::with_envvar(c(Sys.Date = as.Date("2023-09-04")), {
    result <- get_last_registry_entry_before_today("dummy_registry")
    expect_equal(result, as.Date("2023-09-03"))
  })
})
