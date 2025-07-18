library(testthat)
loadNamespace("stringr")

test_that("generate_tt_email works correctly", {

  # Setup test params
  test_root <- rprojroot::find_root(rprojroot::is_testthat)
  program <- "COVID"
  attachments <- dir(file.path(test_root, "test_data"),
                     full.names = TRUE) |>
    stringr::str_subset("_COVID_")
  devs <- readLines("c:/RStudio_Projects/Trialtracker/secrets/devs.csv")
  users <- readLines("c:/RStudio_Projects/Trialtracker/secrets/users.csv")

  # Create email (dev version)
  email <- generate_tt_email(program, attachments, dev_flag = TRUE,
                             devs = devs,
                             users = users)

  # Check the 'from' field
  expect_equal(email$headers$From$values$email, "robert.willans@nice.org.uk")

  # Check the 'to' field
  expect_equal(email$headers$To$values$email, devs)

  # Check the 'Cc' field
  expect_equal(email$headers$Cc$values$email, devs)

  # Check the 'subject' field
  expect_equal(as.character(email$headers$Subject$values), "Trial Tracking Changes - COVID - DEV VERSION")

  # Check attachments
  expect_equal(length(email$parts) - 1, length(attachments))
  expect_equal(email$parts[[1]]$filename, "EU_COVID_Registry_Changes-2024-09-04.xlsx")
  expect_equal(email$parts[[2]]$filename, "ISRCTN_COVID_Registry_Changes-2024-09-04.xlsx")

  # Test without attachments (dev version)
  attachments <- NULL
  email <- generate_tt_email(program, attachments, dev_flag = TRUE,
                             devs = devs,
                             users = users)

  # Check the 'from' field
  expect_equal(email$headers$From$values$email,
               "robert.willans@nice.org.uk")

  # Check the 'to' field
  expect_equal(email$headers$To$values$email, devs)

  # Check the 'Cc' field
  expect_null(email$headers$Cc$values$email)

  # Check the 'subject' field
  expect_equal(as.character(email$headers$Subject$values),
               "TrialTracker ran today - COVID - DEV VERSION")

  # Check (lack of) attachments
  expect_equal(length(email$parts) - 1, 0)

  # Now Test live version
  attachments <- dir(file.path(test_root, "test_data"),
                     full.names = TRUE) |>
    stringr::str_subset("_COVID_")

  email <- generate_tt_email(program, attachments, dev_flag = FALSE,
                             devs = devs,
                             users = users)

  # Check the 'from' field
  expect_equal(email$headers$From$values$email, "robert.willans@nice.org.uk")

  # Check the 'to' field
  expect_equal(email$headers$To$values$email, users)

  # Check the 'Cc' field
  expect_equal(email$headers$Cc$values$email, devs)

  # Check the 'subject' field
  expect_equal(as.character(email$headers$Subject$values), "Trial Tracking Changes - COVID")

  # Check attachments
  expect_equal(length(email$parts) - 1, length(attachments))
  expect_equal(email$parts[[1]]$filename, "EU_COVID_Registry_Changes-2024-09-04.xlsx")
  expect_equal(email$parts[[2]]$filename, "ISRCTN_COVID_Registry_Changes-2024-09-04.xlsx")

  # Check case of no attachments (live version)
  attachments <- NULL
  email <- generate_tt_email(program, attachments, dev_flag = FALSE,
                             devs = devs,
                             users = users)

  # Check the 'from' field
  expect_equal(email$headers$From$values$email, "robert.willans@nice.org.uk")

  # Check the 'to' field
  expect_equal(email$headers$To$values$email, devs)

  # Check the 'Cc' field
  expect_null(email$headers$Cc$values$email)

  # Check the 'subject' field
  expect_equal(as.character(email$headers$Subject$values), "TrialTracker ran today - COVID")

  # Check (lack of) attachments
  expect_equal(length(email$parts) - 1, 0)
})
