########################################################
##    HISTORICAL CLIMATE DATA: MONTHLY SUMMARY        ##
########################################################

# written by Annie Kellner for CEMML 12-13-2024
# annie.kellner@colostate.edu


# This script creates both a monthSum Climate Viewer spreadsheet as well as 
  # the monthSum csv used for reports



# Climate Viewer Column Info

#ScenID <- c(1,2,3,4,5,6,7,8)


#noaa_monthSum <- list()

vars1 <- list()
ppt_list <- list()
vars2 <- list()
noaa_monthSum <- list()

for(i in 1:length(AllDays_hist)){
  df = AllDays_hist[[i]]
  
  df = df %>%
    mutate(MonthNum = month(date)) %>%
    mutate(year = year(date)) 
    #mutate(SITENAME = official_name) %>%  # will add these later, but no need to add to every day
   # mutate(Scenario = "Observed Historical Climate")
  
   yearAvg = df %>%
    dplyr::select(!c('date','PPT_in', 'PPT_mm', 'GDDF')) %>% # exclude variables for which the result is not simply a monthly average
    dplyr::select(!(contains("days"))) %>%
    group_by(year, MonthNum) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) 
  
  Abs_TminF = df %>%
    select(date, year, MonthNum, TMinF) %>%
    group_by(year, MonthNum) %>%
    summarise(Abs_TminF = min(TMinF))
  
  sum_ppt = df %>%
    select(date, year, MonthNum, 'PPT_in', 'PPT_mm') %>%
    group_by(year, MonthNum) %>%
    summarise(across(contains('PPT'), ~ sum(.x, na.rm = TRUE))) %>%
    ungroup()
  
  ppt_list[[i]] <- sum_ppt 
  
  sum_days = df %>%
    select(date, year, MonthNum, contains('days')) %>%
    group_by(year, MonthNum) %>%
    summarise(across(contains('days'), ~ sum(.x, na.rm = TRUE)))
  
  sum_GDDF = df %>%
    select(date, year, MonthNum, GDDF) %>%
    group_by(year, MonthNum) %>%
    summarise(GDDF = sum(GDDF, na.rm = TRUE))
  
  ## new variables added 2-3-2025
  
  Pctl90_TmaxF = df %>%
    select(year, MonthNum, TMaxF) %>%
    group_by(MonthNum) %>%
    summarize(Pctl90_TmaxF = quantile(TMaxF, probs = 0.90, na.rm = TRUE))
  
  Pctl10_TminF = df %>%
    select(year, MonthNum, TMinF) %>%
    group_by(year, MonthNum) %>%
    summarize(Pctl10_TminF = quantile(TMinF, probs = 0.10, na.rm = TRUE))
  
  VHOTDAYS = df %>%
    select(year, MonthNum, TMaxF) %>%
    group_by(year, MonthNum) %>%
    summarize(VHOTDAYS = sum(TMaxF > 95, na.rm = TRUE))
  
  EXHOTDAYS = df %>%
    select(year, MonthNum, TMaxF) %>%
    group_by(year, MonthNum) %>%
    summarize(EXHOTDAYS = sum(TMaxF > 100, na.rm = TRUE))
  
  HELLDAYS = df %>%
    select(year, MonthNum, TMaxF) %>%
    group_by(year, MonthNum) %>%
    summarize(HELLDAYS = sum(TMaxF > 105, na.rm = TRUE))
  
  WARMNIGHTS = df %>%
    select(year, MonthNum, TMinF) %>%
    group_by(year, MonthNum) %>%
    summarize(WARMNIGHTS = sum(TMinF > 75, na.rm = TRUE))
  
  FROSTFREE = df %>%
    select(year, MonthNum, TMinF) %>%
    group_by(year, MonthNum) %>%
    summarize(FROSTFREE = sum(TMinF > 32, na.rm = TRUE))
  
  all = yearAvg %>% # Averages by year (e.g., for Jan 1981, Feb 1981...)
    left_join(sum_days) %>%
    left_join(sum_ppt) %>%
    left_join(Abs_TminF) %>%
    left_join(sum_GDDF) %>%
    left_join(Pctl90_TmaxF) %>%
    left_join(Pctl10_TminF) %>%
    left_join(VHOTDAYS) %>%
    left_join(EXHOTDAYS) %>%
    left_join(HELLDAYS) %>%
    left_join(WARMNIGHTS) %>%
    left_join(FROSTFREE) %>%
    ungroup() 
  

    #rename(year = Avg_year)
    
  Period = paste(first(all$year),"to",last(all$year), sep = " ")
  
  monthAvg = all %>%
    dplyr::select(!year) %>%
    group_by(MonthNum) %>%
    summarise(across(TMaxF:FROSTFREE, ~ mean(.x, na.rm = TRUE)))
  
  monthAvg = monthAvg %>%
    setNames(paste0('Avg_', names(.))) %>%
    rename(MonthNum = Avg_MonthNum) %>%
    mutate(Period = Period)
  
  vars1[[i]] = monthAvg 
 
   }

rm(df)

##  ------  Additional variables derived from summed precip ----------- ##


for(i in 1:length(ppt_list)){
  
  df = select(ppt_list[[i]], year, MonthNum, PPT_in)
  
  Pctl90_Prcp_in = df %>%
    group_by(year, MonthNum) %>%
    summarize("Pctl90_Prcp_in" = quantile(PPT_in, probs = 0.90, na.rm = TRUE))
  
  Pctl10_Prcp_in = df %>%
    group_by(year, MonthNum) %>%
    summarize("Pctl10_Prcp_in" = quantile(PPT_in, probs = 0.10, na.rm = TRUE)) 
  
  VWETDAYS <- df %>%
    group_by(year, MonthNum) %>%
    summarize(VWETDAYS = sum(PPT_in > 4, na.rm = TRUE)) 
  
  quants = left_join(Pctl90_Prcp_in, Pctl10_Prcp_in)
  allSumPPT = left_join(quants, VWETDAYS) %>% ungroup()
  
  monthAvg_PPT = allSumPPT %>%
    #dplyr::select(!year) %>%
    group_by(MonthNum) %>%
    summarise(across(everything(), ~mean(.x, na.rm = TRUE))) 
    
  
  vars2[[i]] = monthAvg_PPT
  
}

rm(df)

# Join all variables


for(i in 1:length(vars1)){
  
  df1 = vars1[[i]]
  df2 = vars2[[i]]
  
  noaa_monthSum[[i]] = left_join(df1, df2, by = "MonthNum")
  
}

rm(df1)
rm(df2)

# Add Month and Period

for(i in 1:length(noaa_monthSum)){
  noaa_monthSum[[i]]$Month = month.abb[noaa_monthSum[[i]]$MonthNum]
  noaa_monthSum[[i]]$Period = paste(first(noaa_monthSum[[i]]$year),"to",last(noaa_monthSum[[i]]$year), sep = " ")
}


monthly = bind_rows(all[[1]], all[[2]], all[[3]])



# Add ScenID

year1 = first(all[[1]]$year)
year2 = first(all[[2]]$year)
year3 = first(all[[3]]$year)


  

  
  
  


  

  
 
  
  newVars = df %>%
    left_join(pptQ90) %>%
    left_join(pptQ10) %>%
    left_join(tmaxFQ90) %>%
    left_join(tminFQ10) %>%
    left_join(Vhot) %>%
    left_join(Xhot) %>%
    left_join(hell) %>%
    left_join(warmNights) %>%
    left_join(frostFree) %>%
    left_join(Vwet)
  
 allVars_monthAvg = newVars %>%
    dplyr::select(!year) %>%
    group_by(month) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
    setNames(paste0('Avg_', names(.))) %>%
    rename(Abs_TminF = Avg_Abs_TminF) %>%
    select(Avg_month, # put in order on MonthSum csv
           Avg_PPT_in, 
           Avg_PPT_mm, 
           Avg_TMaxF, 
           Avg_TMinF, 
           Avg_TMeanF, 
           Abs_TminF, 
           any_of("Avg_hurs"),
           any_of("Avg_sfcWind"),
           Avg_GDDF,
           Avg_hotdays,
           Avg_colddays,
           Avg_wetdays,
           Avg_drydays,
           Avg_ftdays, 
           Avg_pptQ90,
           Avg_pptQ10,
           Avg_Vwet,
           Avg_tmaxFQ90,
           Avg_tminFQ10,
           Avg_Vhot
    ) 
  
  monthAvg = round(monthAvg, digits = 2)
 
  noaa_monthSum[[i]] <- monthAvg 
  
}

    
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

  

  



  



  

  

  

  

  

