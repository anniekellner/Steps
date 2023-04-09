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

rm(list = ls())
# ----------  USER INPUT   -------------------------- #

## Load data (from where AllDays.csv file can be found on the N" drive)
csv_file <- "/Volumes/mindyc/afccm/Climate Modeling/Results_MACA_RH/Homestead_ARB/Homestead_ARB_MACA_historical_1976-2005_AllDays.csv"


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
  dplyr::select(!c('date','PPT_in', 'PPT_mm', 'TMinF')) %>% # Exclude variables for which the result is not simply a monthly average
  dplyr::select(!(contains("days"))) %>%
  group_by(year, month) %>%
  summarise(across(where(is.double), mean)) 

# For TminF, get monthly minumum

tmin <- csv2 %>%
  select(date, year, month, TMinF) %>%
  group_by(year, month) %>%
  summarise(TminF = min(TMinF))

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
  full_join(tmin) %>%
  ungroup()
  
monthAvg <- all %>%
  dplyr::select(!year) %>%
  group_by(month) %>%
  summarise(across(everything(), mean)) %>%
  setNames(paste0('Avg_', names(.))) %>%
  rename(Abs_TminF = Avg_TminF)

data %>% setNames(paste0('cars.', names(.)))
# Create YrAverage row

YrAverage <- rbind(csv, c("YrAverage", colMeans(csv[,2:ncol(csv)]))) # this works but converts columns to characters 

YrAverage <- YrAverage %>%
  mutate_at(c(2:ncol(YrAverage)), as.numeric) # convert back to numeric

# Create YrTotals row

YrTotals <- rbind(YrAverage, c("YrTotals", colSums(YrAverage[,2:ncol(YrAverage)])))

YrTotals <- YrTotals %>%
  mutate_at(c(2:ncol(YrAverage)), as.numeric) %>% # included assuming the results in the .csv should be numeric
  replace(if_else(
    
  ))

## row numbers - assign row_number or something like that, then use that number in combo with na_if() or replace()

# Write to .csv

monthSum_filename <- str_replace(basename(csv_file), "AllDays", "MonthSum")

write_csv(YrTotals, file = paste(dir_output_csvs, monthSum_filename, sep = ""))
