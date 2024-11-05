library(testthat)
library(mockery)
library(dplyr)

# Mock function to replace dbReadTable
mock_dbReadTable <- function(con, registry) {
  data.frame(Query_Date = as.Date(c("2023-09-01", "2023-09-02", "2023-09-03", "2023-09-04", "2099-12-12")))
}

# Test case
testthat::test_that("get_last_registry_entry_before_today returns the correct date", {

  # Mock the dbReadTable function
  mockery::stub(get_last_registry_entry_before_today, "DBI::dbReadTable", mock_dbReadTable)

  # Create a mock connection object
  mock_con <- "mock_connection"

  # Set the system date to a known value
  withr::with_envvar(c(Sys.Date = as.Date("2023-09-04")), {
    result <- get_last_registry_entry_before_today(mock_con, "dummy_registry")
    expect_equal(result, as.Date("2023-09-03"), tolerance = 0.0001)
  })
})
