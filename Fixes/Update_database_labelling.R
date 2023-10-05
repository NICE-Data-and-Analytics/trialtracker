
library(DBI)
library(tidyverse)

con <- dbConnect(RSQLite::SQLite(), "RSQLite_Data/TrialTracker-db.sqlite")

# Show available tables in database
DBI::dbListTables(con)

# Which need to be updated? 
# Trial_IDs
# NCT, NCT_PM, NCT_rxv (one trial has been archived)
# ISRCTN, not ISRCTN_PM (no values), not ISRCTN_rxv (no values)

NCT_studies_to_update <- c("NCT04757857", "NCT05780268", "NCT05780281", "NCT05780424", "NCT05780437", "NCT05986422")
ISRCTN_studies_to_update <- c("ISRCTN30448031")


# Update Trial_Ids table --------------------------------------------------

# Read in Trial Ids table
Trial_Ids <- dbReadTable(con, "Trial_Ids")

# Update actual table
updated_Trial_Ids <- Trial_Ids |> 
  mutate(Program = replace(Program,
                           NCT_Ids %in% NCT_studies_to_update |ISRCTN_Ids %in% ISRCTN_studies_to_update,
                           "COVID"),
         # Also updated guideline number to "COVID"
         Guideline.number = replace(Guideline.number,
                                    NCT_Ids %in% NCT_studies_to_update |ISRCTN_Ids %in% ISRCTN_studies_to_update,
                                    "COVID"))

# Remove current table and replace with updated version 
dbWriteTable(con, "Trial_Ids", updated_Trial_Ids, overwrite = TRUE)


# Update NCT table --------------------------------------------------

# Read in Trial Ids table
NCT <- dbReadTable(con, "NCT") |> 
  as_tibble()

updated_NCT <- NCT |> 
  mutate(Program = replace(Program,
                           NCTId %in% NCT_studies_to_update,
                           "COVID"),
         # Also updated guideline number to "COVID"
         Guideline.number = replace(Guideline.number,
                                    NCTId %in% NCT_studies_to_update,
                                    "COVID"))

DBI::dbWriteTable(con, "NCT", updated_NCT, overwrite = TRUE)


# Update NCT PM table --------------------------------------------------

NCT_PM <- dbReadTable(con, "NCT_PM") |> 
  as_tibble()

updated_NCT_PM <- NCT_PM |> 
  mutate(Program = replace(Program,
                           ID %in% NCT_studies_to_update,
                           "COVID"),
         # Also updated guideline number to "COVID"
         Guideline.number = replace(Guideline.number,
                                    ID %in% NCT_studies_to_update,
                                    "COVID"))

DBI::dbWriteTable(con, "NCT", updated_NCT, overwrite = TRUE)


# Update NCT_rxv table --------------------------------------------------

# Read in Trial Ids table
NCT_rxv <- dbReadTable(con, "NCT_rxv") |> 
  as_tibble()

updated_NCT_rxv <- NCT_rxv |> 
  mutate(Program = replace(Program,
                           NCTId %in% NCT_studies_to_update,
                           "COVID"),
         # Also updated guideline number to "COVID"
         Guideline.number = replace(Guideline.number,
                                    NCTId %in% NCT_studies_to_update,
                                    "COVID"))

DBI::dbWriteTable(con, "NCT", updated_NCT, overwrite = TRUE)


# Update ISRCTN table --------------------------------------------------

ISRCTN <- dbReadTable(con, "ISRCTN") |> 
  as_tibble()

updated_ISRCTN <- ISRCTN |> 
  mutate(Program = replace(Program,
                           ISRCTN_No %in% ISRCTN_studies_to_update,
                           "COVID"),
         # Also updated guideline number to "COVID"
         Guideline.number = replace(Guideline.number,
                                    ISRCTN_No %in% ISRCTN_studies_to_update,
                                    "COVID"))

DBI::dbWriteTable(con, "ISRCTN", updated_ISRCTN, overwrite = TRUE)
