########################################################
##    HISTORICAL CLIMATE DATA: MONTHLY SUMMARY        ##
########################################################

# written by Annie Kellner for CEMML 12-13-2024
# annie.kellner@colostate.edu


# This script creates both a monthSum Climate Viewer spreadsheet as well as 
  # the monthSum csv used for reports


vars <- list()

for(i in 1:length(AllDays_hist)){
  df = AllDays_hist[[i]]
  
  df = df %>%
    mutate(MonthNum = month(date)) %>%
    mutate(year = year(date)) 
    #mutate(SITENAME = official_name) %>%  # will add these later, but no need to add to every day
   # mutate(Scenario = "Observed Historical Climate")
  
   yearAvg = df %>%
    dplyr::select(!c('date','PPT_in', 'GDDF')) %>% # exclude variables for which the result is not simply a monthly average
    dplyr::select(!(contains("days"))) %>%
    dplyr::select(!(contains("DAYS"))) %>%
    select(!contains("NIGHTS")) %>% 
    group_by(year, MonthNum) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
     ungroup()
  
  Abs_TminF = df %>%
    select(date, year, MonthNum, TMinF) %>%
    group_by(year, MonthNum) %>%
    summarise(Abs_TminF = min(TMinF)) %>%
    ungroup()
  
  sum_ppt = df %>%
    select(date, year, MonthNum, 'PPT_in') %>%
    group_by(year, MonthNum) %>%
    summarise(across(contains('PPT'), ~ sum(.x, na.rm = TRUE))) %>%
    ungroup()
  
  #ppt_list[[i]] <- sum_ppt 
  
  sum_days = df %>%
    select(date, year, MonthNum, contains('days')) %>%
    group_by(year, MonthNum) %>%
    summarise(across(contains('days'), ~ sum(.x, na.rm = TRUE))) %>%
    ungroup()
  
  sum_DAYS = df %>%
    select(date, year, MonthNum, contains('DAYS')) %>%
    group_by(year, MonthNum) %>%
    summarise(across(contains('DAYS'), ~ sum(.x, na.rm = TRUE))) %>%
    ungroup()
  
  sum_nights = df %>%
    select(date, year, MonthNum, contains('NIGHTS')) %>%
    group_by(year, MonthNum) %>%
    summarise(across(contains('nights'), ~ sum(.x, na.rm = TRUE))) %>%
    ungroup()
  
  sum_GDDF = df %>%
    select(date, year, MonthNum, GDDF) %>%
    group_by(year, MonthNum) %>%
    summarise(GDDF = sum(GDDF, na.rm = TRUE)) %>%
    ungroup()
  
  ## new variables added 2-3-2025
  
  all = yearAvg %>% # Averages by year (e.g., for Jan 1981, Feb 1981...)
    left_join(sum_days) %>%
    left_join(sum_ppt) %>%
    left_join(Abs_TminF) %>%
    left_join(sum_GDDF) %>%
    left_join(sum_DAYS) %>%
    left_join(sum_nights)
    
  Period = paste(first(all$year),"to",last(all$year), sep = " ")
  
  monthAvg = all %>%
    dplyr::select(!year) %>%
    group_by(MonthNum) %>%
    summarise(across(TMaxF:WARMNIGHTS, ~ mean(.x, na.rm = TRUE))) %>%
    round(digits = 1) %>%
    ungroup()
  
  monthAvg = monthAvg %>%
    setNames(paste0('Avg_', names(.))) %>%
    rename(MonthNum = Avg_MonthNum) %>%
    rename(Avg_Prcp_in = Avg_PPT_in) %>%
    rename(Avg_TmaxF = Avg_TMaxF) %>%
    rename(Avg_TmeanF = Avg_TMeanF) %>%
    rename(Avg_TminF = Avg_TMinF) %>%
    rename(HOTDAYS = Avg_hotdays) %>%
    rename(VHOTDAYS = Avg_VHOTDAYS) %>%
    rename(EXHOTDAYS = Avg_EXHOTDAYS) %>%
    rename(HELLDAYS = Avg_HELLDAYS) %>%
    rename(WARMNIGHTS = Avg_WARMNIGHTS) %>%
    rename(COLDDAYS = Avg_colddays) %>%
    rename(FRFRDAYS = Avg_FRFRDAYS) %>%
    rename(FTDAYS = Avg_ftdays) %>%
    rename(GDDF = Avg_GDDF) %>%
    rename(DRYDAYS = Avg_drydays) %>%
    rename(WETDAYS = Avg_wetdays) %>%
    rename(VWETDAYS = Avg_VWETDAYS)
  
  Pctl90_TmaxF = df %>%
    select(MonthNum, TMaxF) %>%
    group_by(MonthNum) %>%
    summarize(Pctl90_TmaxF = quantile(TMaxF, probs = 0.90, na.rm = TRUE)) %>%
    ungroup()
  
  Pctl10_TminF = df %>%
    select(MonthNum, TMinF) %>%
    group_by(MonthNum) %>%
    summarize(Pctl10_TminF = quantile(TMinF, probs = 0.10, na.rm = TRUE)) %>%
    ungroup()
  
  Pctl90_Prcp_in = sum_ppt %>%
    group_by(MonthNum) %>%
    summarize(Pctl90_Prcp_in = quantile(PPT_in, probs = 0.90, na.rm = TRUE)) %>%
    ungroup()
  
  Pctl10_Prcp_in = sum_ppt %>%
    group_by(MonthNum) %>%
    summarize(Pctl10_Prcp_in = quantile(PPT_in, probs = 0.10, na.rm = TRUE)) %>%
    ungroup()
  
  monthAvg = monthAvg %>%
    left_join(Pctl90_TmaxF) %>%
    left_join(Pctl10_TminF) %>%
    left_join(Pctl90_Prcp_in) %>%
    left_join(Pctl10_Prcp_in)
  
  monthAvg$Period <- Period
  
  vars[[i]] = monthAvg 
 
   }

rm(df)


# Add Scenario ID (hard-coded for now)

vars[[1]]$ScenID <- "1"
vars[[2]]$ScenID <- "2"
vars[[3]]$ScenID <- "3"


# --------  JOIN INTO SINGLE DATAFRAME  ---------- #

noaaMonthly <- bind_rows(list(vars[[1]], vars[[2]], vars[[3]]))


## Add columns common to all 

noaaMonthly$Month <- month.abb[noaaMonthly$MonthNum]
noaaMonthly$SITENAME <- official_name
noaaMonthly$Scenario <- "Observed Historical Climate"


## Arrange columns

noaaMonthly <- noaaMonthly %>%
  select(SITENAME,
          Scenario,
          Period,
          ScenID,
          Month,
          MonthNum,
          Pctl90_Prcp_in,
          Avg_Prcp_in,
          Pctl10_Prcp_in,
          Pctl90_TmaxF,
          Avg_TmaxF,
          Avg_TmeanF,
          Avg_TminF,
          Pctl10_TminF,
          HOTDAYS,
          VHOTDAYS,
          EXHOTDAYS,
          HELLDAYS,
          WARMNIGHTS,
          COLDDAYS,
          FRFRDAYS,
          FTDAYS,
          GDDF,
          DRYDAYS,
          WETDAYS,
          VWETDAYS)
          
          
    



  
  


  


    
# Add summary rows (YrAverage and YrTotals) and save for table construction

Avs_and_Totals <- list() # saving for summary table

for(i in 1:length(noaa_monthSum)){
  YrAverage = colMeans(noaa_monthSum[[i]][,2:ncol(noaa_monthSum[[i]])]) # converts columns to characters 
  YrTotals = colSums(noaa_monthSum[[i]][,2:ncol(noaa_monthSum[[i]])])
  csv = bind_rows(noaa_monthSum[[i]], YrAverage, YrTotals)

  # NA's 
  
  NAs_Avgs = csv %>% # YrAverage
    filter(row_number() == 13) %>% # Because there will always be 12 months irrespective of model
    mutate(across(.cols = contains("PPT"), ~na_if(.,.))) %>%
    mutate(across(.cols = contains("days"), ~na_if(.,.))) %>%
    mutate(across(.cols = contains("Abs"), ~na_if(.,.))) %>%
    mutate(across(.cols = contains("GDDF"), ~na_if(.,.))) 


  NAs_Totals = csv %>% # YrTotals
    filter(row_number() == 14) %>%
    mutate(across(.cols = contains("Avg_T"), ~na_if(.,.))) %>%
    mutate(across(.cols = contains("Abs"), ~na_if(.,.))) %>%
    mutate(across(.cols = any_of("Avg_hurs"), ~na_if(.,.))) %>%
    mutate(across(.cols = any_of("Avg_sfcWind"), ~na_if(.,.))) 
  
  csv = csv %>%
    slice(1:(n()-2)) %>% # remove summary rows
    bind_rows(NAs_Avgs, NAs_Totals) %>% # replace with NA's included
    rename(month = Avg_month) %>%
    mutate(month = as.character(month))
  
  csv[13,1] = "YrAverage"
  csv[14,1] = "YrTotals"
  
  csv -> Avs_and_Totals[[i]] 

}

# Save .csv's to Results folder

# Group 1

fileName_grp1 <- paste(weather_station,"1981-2010","historical","MonthSum", sep = "_")
filePath_grp1 <- paste0(noaa_resultsDir,"/",fileName_grp1,".csv")

write_csv(Avs_and_Totals[[1]], file = filePath_grp1)

# Group 2

fileName_grp2 <- paste(weather_station,"1985-2014","historical","MonthSum", sep = "_")
filePath_grp2 <- paste0(noaa_resultsDir,"/",fileName_grp2,".csv")

write_csv(Avs_and_Totals[[2]], file = filePath_grp2)

# Group 3

fileName_grp3 <- paste(weather_station,"1991-2020","historical","MonthSum", sep = "_")
filePath_grp3 <- paste0(noaa_resultsDir,"/",fileName_grp3,".csv")


write_csv(Avs_and_Totals[[3]], file = filePath_grp3)

  

  



  



  

  

  

  

  

