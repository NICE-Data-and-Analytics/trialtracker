library(tidyverse)
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), "RSQLite_Data/TrialTracker-db.sqlite")

add_comments_to_table <- function(table_name, con) {
  
  table <-
    dbReadTable(con, table_name) %>% 
    add_column("Comments" = NA)
  
  dbWriteTable(con, table_name, table, overwrite = TRUE)
  
}

list_of_tables <- c("EU", "EU_rxv", "ISRCTN", "ISRCTN_rxv", "NCT", "NCT_rxv", "NIHR", "NIHR_rxv")

map(list_of_tables, add_comments_to_table, con = con)