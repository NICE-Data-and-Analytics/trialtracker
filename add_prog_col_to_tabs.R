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

write_prog_col_to_df <- function(sql_table){
  
  df <- dbReadTable(con, sql_table)
  
  df <- df %>%
    mutate("Program" = case_when(Guideline.number == "COVID" ~ "COVID",
                                 Guideline.number == "NG191" ~ "COVID",
                                 Guideline.number == "NG188" ~ "COVID",
                                 str_detect(Guideline.number, 'IPG[0-9]*') ~ "Other",
                                 str_detect(Guideline.number, 'IP[0-9]*') ~ "IP",
                                 TRUE ~ "Other")
    )
  
  dbWriteTable(con, sql_table, df, overwrite = TRUE)
  
}
