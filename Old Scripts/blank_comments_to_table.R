library(tidyverse)
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), "RSQLite_Data/TrialTracker-db.sqlite")

blank_comments_to_table <- function(table_name, con) {
  
  table <-
    dbReadTable(con, table_name) %>% 
    mutate("Comments" = "")
  
  dbWriteTable(con, table_name, table, overwrite = TRUE)
  
}

list_of_tables <- c("EU", "ISRCTN", "NCT", "NIHR", "EU_rxv", "ISRCTN_rxv", "NCT_rxv", "NIHR_rxv")

map(list_of_tables, blank_comments_to_table, con = con)