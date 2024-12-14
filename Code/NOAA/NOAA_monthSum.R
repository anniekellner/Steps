########################################################
##    HISTORICAL CLIMATE DATA: MONTHLY SUMMARY        ##
########################################################

# written by Annie Kellner for CEMML 12-13-2024
# annie.kellner@colostate.edu


noaa_monthSum <- list()


for(i in 1:length(AllDays_hist)){
  df = AllDays_hist[[i]]
  df = df %>%
    mutate(month = month(date)) %>%
    mutate(year = year(date)) 
  
  yearAvg = df %>%
    dplyr::select(!c('date','PPT_in', 'PPT_mm', 'GDDF')) %>% # Exclude variables for which the result is not simply a monthly average
    dplyr::select(!(contains("days"))) %>%
    group_by(year, month) %>%
    summarise(across(where(is.numeric), mean))
  
  Abs_TminF = df %>%
    select(date, year, month, TMinF) %>%
    group_by(year, month) %>%
    summarise(Abs_TminF = min(TMinF))
  
  sum_ppt = df %>%
    select(date, year, month, 'PPT_in', 'PPT_mm') %>%
    group_by(year, month) %>%
    summarise(across(contains('PPT'), sum))
  
  sum_days = df %>%
    select(date, year, month, contains('days')) %>%
    group_by(year, month) %>%
    summarise(across(contains('days'), sum))
  
  sum_GDDF = df %>%
    select(date, year, month, GDDF) %>%
    group_by(year, month) %>%
    summarise(GDDF = sum(GDDF))
  
  all = yearAvg %>%
    full_join(sum_days) %>%
    full_join(sum_ppt) %>%
    full_join(Abs_TminF) %>%
    full_join(sum_GDDF) %>%
    ungroup() 
  
  monthAvg = all %>%
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
           any_of("Avg_hurs"),
           any_of("Avg_sfcWind"),
           Avg_GDDF,
           Avg_hotdays,
           Avg_colddays,
           Avg_wetdays,
           Avg_drydays,
           Avg_ftdays
    ) 
 
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
    bind_rows(NAs, NAs_Totals) %>% # replace with NA's included
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

write.csv(grp1, file = filePath_grp1)

# Group 2

fileName_grp2 <- paste(weather_station,"1985-2014","historical","MonthSum", sep = "_")
filePath_grp2 <- paste0(noaa_resultsDir,"/",fileName_grp2,".csv")

write.csv(grp2, file = filePath_grp2)

# Group 3

fileName_grp3 <- paste(weather_station,"1991-2020","historical","MonthSum", sep = "_")
filePath_grp3 <- paste0(noaa_resultsDir,"/",fileName_grp3,".csv")


#### STOPPED HERE BC FEEL LIKE SHIT
write.csv(grp3, file = filePath_grp3)

  
write.csv(Avs_and_Totals[[1]], )  
  
## MONTHSUM CODE FROM RMD SCRIPT


  



  

  

  

  

  

  
  #Avs_and_Totals[[i]] = csv %>% slice_tail(n = 2) 
  #names(Avs_and_Totals)[i] = names(AllDays[i])
  
 # monthSum[[i]] = csv # will write over original monthSum df that did not have final two columns
  #names(monthSum)[i] = names(AllDays[i])
#}
