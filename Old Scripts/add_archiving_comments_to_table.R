library(tidyverse)
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), "RSQLite_Data/TrialTracker-db.sqlite")

add_archiving_comments_to_table <- function(table_name, con) {
  
  table <-
    dbReadTable(con, table_name) %>% 
    add_column("Archiving_Comments" = NA)
  
  dbWriteTable(con, table_name, table, overwrite = TRUE)
  
}

list_of_tables <- c("EU_rxv", "ISRCTN_rxv", "NCT_rxv", "NIHR_rxv")

map(list_of_tables, add_archiving_comments_to_table, con = con)