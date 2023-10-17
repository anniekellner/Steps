###############################################
#######      MonthSum   #######################
###############################################

# Written by Annie Kellner 4/9/23 
# Contact: annie.kellner@colostate.edu

# The input for this script is a .csv file created by Step3.Rmd (AllDays.csv);
# The .csv file is then altered manually by Trevor Even
# Outp
# Eventually, this script will be incorporated into Step3.Rmd and no .csv files will be used for input

library(dplyr)
library(readr)
library(lubridate)
library(stringr)

rm(list = ls())

# ----------  USER INPUT   -------------------------- #

## Load data (from where AllDays.csv file can be found on the N" drive)
csv_file <- "/Volumes/mindyc/afccm/Climate Modeling/Results_MACA_RH/Homestead_ARB/Homestead_ARB_MACA_historical_1976-2005_AllDays.csv"

dir_output_csvs = './Results/Test/' 

# ------ AUTOMATED -------------------------------- #

options(dplyr.summarise.inform = FALSE) # This is to avoid meaningless messages from dplyr. Not sure if it needs to be run every time. 

csv <- read_csv(csv_file, show_col_types = FALSE) 

# Change Excel's exported date format (character) and add columns for month and year. Delete lat/lon info

csv2 <- csv %>% 
  mutate(date = mdy(date)) %>%
  mutate(month = month(date)) %>%
  mutate(year = year(date)) %>%
  dplyr::select(-c(lat, lon))

# Get monthly averages for all data for which a simple monthly average is desired

yearAvg <- csv2 %>%
  dplyr::select(!c('date','PPT_in', 'PPT_mm')) %>% # Exclude variables for which the result is not simply a monthly average
  dplyr::select(!(contains("days"))) %>%
  group_by(year, month) %>%
  summarise(across(where(is.double), mean)) 

# For Abs_TminF, get monthly minumum

Abs_TminF <- csv2 %>%
  select(date, year, month, TMinF) %>%
  group_by(year, month) %>%
  summarise(Abs_TminF = min(TMinF))

# For precip variables, get monthly sum

ppt <- csv2 %>%
  select(date, year, month, 'PPT_in', 'PPT_mm') %>%
  group_by(year, month) %>%
  summarise(across(contains('PPT'), sum))

# For 'days' variables (e.g., hotdays, colddays), get monthly sum

days <- csv2 %>%
  select(date, year, month, contains('days')) %>%
  group_by(year, month) %>%
  summarise(across(contains('days'), sum))

# Join yearAvg, ppt, TminF, and days

all <- yearAvg %>%
  full_join(days) %>%
  full_join(ppt) %>%
  full_join(Abs_TminF) %>%
  ungroup()
  
monthAvg <- all %>%
  dplyr::select(!year) %>%
  group_by(month) %>%
  summarise(across(everything(), mean)) %>%
  setNames(paste0('Avg_', names(.))) %>%
  rename(Abs_TminF = Avg_Abs_TminF) %>%
  select(Avg_month, # put in order on MonthSum csv
         Avg_PPT_in, 
         Avg_PPT_mm, 
         Avg_TMaxF, 
         Avg_TMinF, 
         Avg_TMeanF, 
         Abs_TminF, 
         Avg_GDDF,
         Avg_hotdays,
         Avg_colddays,
         Avg_wetdays,
         Avg_drydays,
         Avg_ftdays,
         Avg_Rad,
         Avg_EastWind,
         Avg_NorthWind,
         Avg_WindSpeed,
         Avg_WindDirDeg,
         Avg_SH,
         Avg_VPdeficit,
         Avg_VPsat,
         Avg_VPamb,
         Avg_RH)
  
# Create YrAverage row

YrAverage <- colMeans(monthAvg[,2:ncol(monthAvg)]) # converts columns to characters 
YrTotals <- colSums(monthAvg[,2:ncol(monthAvg)])

MonthSum <- bind_rows(monthAvg, YrAverage, YrTotals)

# NA's 

NAs <- MonthSum %>% # YrAverage
  filter(row_number() == 13) %>%
  mutate(across(.cols = contains("PPT"), ~na_if(.,.))) %>%
  mutate(across(.cols = contains("days"), ~na_if(.,.))) %>%
  mutate(across(.cols = contains("Abs"), ~na_if(.,.))) %>%
  mutate(across(.cols = contains("GDDF"), ~na_if(.,.))) 

NAs_Totals <- MonthSum %>% # YrTotals
  filter(row_number() == 14) %>%
  mutate(across(.cols = contains("Avg_T"), ~na_if(.,.))) %>%
  mutate(across(.cols = contains("Abs"), ~na_if(.,.))) %>%
  mutate(across(.cols = Avg_Rad:Avg_RH, ~na_if(.,.))) # assuming these columns stay in the same order

           
MonthSum2 <- MonthSum %>%
  slice(1:(n()-2)) %>% # remove summary rows
  bind_rows(NAs, NAs_Totals) %>% # replace with NA's included
  rename(month = Avg_month) %>%
  rename(Avg_TminF = Avg_TMinF) %>%
  mutate(month = as.character(month))

MonthSum2[13,1] <- "YrAverage"
MonthSum2[14,1] <- "YrTotals"

# Write to .csv

monthSum_filename <- str_replace(basename(csv_file), "AllDays", "MonthSum")

write_csv(MonthSum2, file = paste(dir_output_csvs, monthSum_filename, sep = ""))
