# load packages
library(dplyr)
library(RMariaDB)
library(ggplot2)
library(readxl)
library(lubridate)
library(dbplyr)
library(binom)
library(readr)
library(ggpubr)
library(formattable)

# make MySQL connection
setwd("C:/Users/chase/OneDrive/Documents/Razorback_Sucker")
MySQLSettings <- "C:/Users/chase/OneDrive/Documents/R/PIT tag database/brk828_nativef1_sensing.cnf"
con <- dbConnect(RMariaDB::MariaDB(),default.file=MySQLSettings, group="settings")
dbListTables(con)

# Download source material
scan_db <- tbl(con, "scan_data")
effort_db <- tbl(con, "scan_effort")
location_db <- tbl(con, "location")
release_db <- tbl(con, "WebTable")
dbListFields(con, 'WebTable')
locations <- dbReadTable(con, 'location')

#Connecting tables and data wrangling to get stocked fish and scan history from Razorback in Reaches 2 and 3
ScanningTable <- release_db %>%
  select(LID, DISP_DATE, PIT = TAG_NUMBER, STATUS, SPECIES_ID, TL) %>% 
  filter(STATUS == 'Release', SPECIES_ID == 'XYTE') %>%
  inner_join(location_db %>% select(LID = LOCATION_ID, MSCP_REACH, ZONE, LOCATION) %>%
               filter(MSCP_REACH %in% c(2,3,4)), by = "LID") %>%
  left_join(scan_db %>% select(PIT, date), by = 'PIT') %>%
  collect()
ScanningTable <- ScanningTable %>% mutate(Release_Date = mdy(DISP_DATE)) %>% select(-DISP_DATE)

#set maximum date of 2 years prior to stocking and minimum date as fish that have been stocked within the last 10 years
MaximumDate <- Sys.Date() - years(2)
MinimumDate <- Sys.Date() - years(10)

#Create new table to add 0's and 1's and season based on if the fish was scanned at least 1 year post stocking
XYTE <- ScanningTable %>% filter(Release_Date >= MinimumDate & Release_Date <= MaximumDate) %>% 
  group_by(PIT, TL, Release_Date, MSCP_REACH, ZONE) %>% summarize(Scan_Date = max(date)) %>%
  mutate(Survivor =
           case_when(is.na(Scan_Date) ~ 0,
                     Scan_Date >= Release_Date + 365 ~ 1,
                     Scan_Date < Release_Date + 365 ~ 0)) %>%
  mutate(ReleaseSeason = 
           case_when(format(Release_Date, format = "%m") %in% c("9", "10", "11") ~ "pre-spawn",
                     format(Release_Date, format = "%m") %in% c("12", "01", "02", "03") ~ "spawn",
                     format(Release_Date, format = "%m") %in% c("04", "05") ~ "post-spawn")) %>% 
  select(-Scan_Date, -Release_Date) %>% na.omit() %>% mutate(MSCP_REACH = as.factor(MSCP_REACH))

#save data
saveRDS(XYTE, 'XYTE_scan_data.rds')