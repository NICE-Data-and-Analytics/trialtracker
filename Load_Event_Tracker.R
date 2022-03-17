# Script to load event tracker numbers and save to SQL table

#Libraries
library(tidyverse)
library(readxl)
library(DBI)

setwd('//srv/shiny-server/trialtracker-dev')

#Set Regex patterns
NCT_pattern <- "NCT[0-9]{8}"
EU_pattern <- "[0-9]{4}-[0-9]{6}-[0-9]{2}"
ISRCTN_pattern <- "ISRCTN[0-9]{8}"
NIHR_pattern <- "((?<=hta/|hsdr/|eme/|phr/)[0-9]{6,8})|(RP-PG-[0-9]{4}-[0-9]{4,5})"

#load from xlsx
Event_Tracker <-
  read_excel(
    "Data_Files/Event tracker_working version_Dec19.xlsx",
    col_types = c("text", "text", "skip", "skip", "skip", "text", "skip", "skip", "skip",
                  "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip")) %>% 
  rename(URL = `Link to event website`) %>% 
  mutate("NCT_Ids" = str_extract(URL, NCT_pattern),
         "EU_Ids" = str_extract(URL, EU_pattern),
         "ISRCTN_Ids" = str_extract(URL, ISRCTN_pattern),
         "NIHR_Ids" = str_extract(URL, NIHR_pattern)) %>%
  mutate("NIHR_Ids" = gsub("^(\\d{2})(\\d{2,3})(\\d{2,3})$", "\\1-\\2-\\3", NIHR_Ids)) %>%
  pivot_longer(NCT_Ids:NIHR_Ids, values_drop_na = TRUE) %>%
  rownames_to_column() %>% 
  pivot_wider(names_from = name) %>% 
  select(-rowname)

#Go back and manually correct NIHR ones
Event_Tracker <-
  Event_Tracker %>% mutate(NIHR_Ids = case_when(NIHR_Ids == "15-571-60" ~ "15-57-160",
                                                NIHR_Ids == "13-041-08" ~ "13-04-108",
                                                NIHR_Ids == "11-300-201" ~ "11-3002-01",
                                                NIHR_Ids == "14-491-49" ~ "14-49-149",
                                                NIHR_Ids == "14-701-53" ~ "14-70-153",
                                                NIHR_Ids == "07-365-01" ~ "07-36-501",
                                                NIHR_Ids == "10-300-613" ~ "10-3006-13",
                                                NIHR_Ids == "11-300-211" ~ "11-3002-11",
                                                NIHR_Ids == "15-701-01" ~ "15-70-101",
                                                NIHR_Ids == "RP-PG-0311-12001" ~ "DTC-RP-PG-0311-12001",
                                                NIHR_Ids == "14-701-62" ~ "14-70-162",
                                                TRUE ~ NIHR_Ids
                                                ))

#Save to csv file
write_csv(Event_Tracker, "Data_Files/Events_Tracked.csv")

#Load file to RSQLite DB (in DB folder)
con <- dbConnect(RSQLite::SQLite(), "RSQLite_Data/TrialTracker-db.sqlite")
RSQLite::dbWriteTable(con, "Trial_Ids",  Event_Tracker, overwrite = TRUE)
dbDisconnect(con)

#clear env
rm(list = ls())