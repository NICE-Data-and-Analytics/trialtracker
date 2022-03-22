library(tidyverse)
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), "RSQLite_Data/TrialTracker-db.sqlite")

remove_comments_from_table <- function(table_name, con) {
  
  table <-
    dbReadTable(con, table_name) %>% 
    select(-contains("Comments."))
  
  dbWriteTable(con, table_name, table, overwrite = TRUE)
  
}

list_of_tables <- c("EU", "EU_rxv", "ISRCTN", "ISRCTN_rxv", "NCT", "NCT_rxv", "NIHR", "NIHR_rxv")

map(list_of_tables, remove_comments_from_table, con = con)