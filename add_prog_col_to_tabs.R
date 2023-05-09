## Function to add Program

add_program_col_to_tibble <- function(tibble){
  
  tibble %>% 
    add_column("Program" = NA, .before = "Guideline.number")
  
}

add_program_col_to_sql_table <- function(table, con){
  
  tibble <- dbReadTable(con, table) %>% 
    add_program_col_to_tibble()
  
  dbWriteTable(conn = con, name = table, value = tibble, overwrite = TRUE)
  
}
