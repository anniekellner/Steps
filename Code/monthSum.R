###############################################################################
###         MONTHLY SUMMARY OF VARIABLES                                    ###
###############################################################################

# amended by Annie Kellner (annie.kellner@colostate.edu) 2-6-2025

# This script produces the following outputs:
  # monthSum.csv for traditional use with written reports
  # Monthly Summary formatted for Climate Viewer input
  # monthSum dataframe for use in downstream analyses

# Inputs: 
  # AllDays dataframe (tibble)



###   BEGIN SCRIPT  ###


monthSumDF <- list()


for(i in 1:length(AllDays)){
  df = AllDays[[i]]
  df = df %>%
    mutate(date = ymd(date)) %>%
    mutate(month = month(date)) %>%
    mutate(year = year(date)) %>%
    dplyr::select(-c(lat, lon, tmax, tmin, prcp))
  
  yearAvg = df %>%
    dplyr::select(!c('date','PPT_in', 'PPT_mm', 'GDDF')) %>% # exclude variables for which the result is not simply a monthly average
    dplyr::select(!(contains("days"))) %>%
    group_by(year, month) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
    ungroup()
  
  Abs_TminF = df %>%
    select(date, year, month, TMinF) %>%
    group_by(year, month) %>%
    summarise(Abs_TminF = min(TMinF)) %>%
    ungroup()
  
  sum_ppt = df %>%
    select(date, year, month, 'PPT_in', 'PPT_mm') %>%
    group_by(year, month) %>%
    summarise(across(contains('PPT'), ~ sum(.x, na.rm = TRUE))) %>%
    ungroup()
  
  sum_days = df %>%
    select(date, year, month, contains('days')) %>%
    group_by(year, month) %>%
    summarise(across(contains('days'), ~ sum(.x, na.rm = TRUE))) %>%
    ungroup()
  
  sum_GDDF = df %>%
    select(date, year, month, GDDF) %>%
    group_by(year, month) %>%
    summarise(GDDF = sum(GDDF, na.rm = TRUE)) %>%
    ungroup()
  
  all = yearAvg %>%
    left_join(sum_days) %>%
    left_join(sum_ppt) %>%
    left_join(Abs_TminF) %>%
    left_join(sum_GDDF) %>%
    ungroup()
  
  monthAvg = all %>%
    dplyr::select(!year) %>%
    group_by(month) %>%
    summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
    round(digits = 1) %>%
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
           Avg_specHum,
           Avg_VPD
    )
  
  monthSumDF[[i]] = monthAvg # new df for export and use with DiffHist
  names(monthSumDF)[i] = names(AllDays[i])
}

# Add summary rows (YrAverage and YrTotals) and save for table construction

Avs_and_Totals <- list() # saving for summary table
monthSum <- list() # with final two summary rows (for csv creation)

for(i in 1:length(monthSumDF)){
  YrAverage = colMeans(monthSumDF[[i]][,2:ncol(monthSumDF[[i]])]) # converts columns to characters 
  YrTotals = colSums(monthSumDF[[i]][,2:ncol(monthSumDF[[i]])])
  csv = bind_rows(monthSumDF[[i]], YrAverage, YrTotals)
  
  # NA's 
  
  NAs = csv %>% # NA's for YrAverage
    filter(row_number() == 13) %>% # Because there will always be 12 months irrespective of model
    mutate(across(.cols = contains("PPT"), ~na_if(.,.))) %>%
    mutate(across(.cols = contains("days"), ~na_if(.,.))) %>%
    mutate(across(.cols = contains("Abs"), ~na_if(.,.))) %>%
    mutate(across(.cols = contains("GDDF"), ~na_if(.,.))) 

  NAs_Totals = csv %>% # NA's for YrTotals
    filter(row_number() == 14) %>%
    mutate(across(.cols = contains("Avg_T"), ~na_if(.,.))) %>%
    mutate(across(.cols = contains("Abs"), ~na_if(.,.))) %>%
    mutate(across(.cols = any_of("Avg_sfcWind"), ~na_if(.,.))) %>%
    mutate(across(.cols = contains("hu"), ~na_if(.,.)))
  
  csv = csv %>%
    slice(1:(n()-2)) %>% # remove summary rows
    bind_rows(NAs, NAs_Totals) %>% # replace with NA's included
    rename(month = Avg_month) %>%
    mutate(month = as.character(month))
  
  csv[13,1] = "YrAverage"
  csv[14,1] = "YrTotals"
  
  Avs_and_Totals[[i]] = csv %>% slice_tail(n = 2) 
  names(Avs_and_Totals)[i] = names(AllDays[i])
  
  monthSum[[i]] = csv # will write over original monthSum df that did not have final two columns
  names(monthSum)[i] = names(AllDays[i])
}

# Write csv files 

for(i in 1:length(monthSum)){
  
  csv_scenario = if (str_detect(names(monthSum[i]), "baseline")){
    "historical"
  } else if(str_detect(names(monthSum[i]),"s1")) {
    scenarios[2]
  } else {
    scenarios[3]
  }
  
  csv_years = if (str_detect(names(monthSum[i]), "f1")){
    paste(first(year(AllDays$results_s1f1$date)),"-",last(year(AllDays$results_s1f1$date)))
    
  } else if (str_detect(names(monthSum[i]), "f2")) {
    paste(first(year(AllDays$results_s1f2$date)),"-",last(year(AllDays$results_s1f2$date)))
  } else {
    paste(first(year(AllDays$results_baseline$date)),"-",last(year(AllDays$results_baseline$date)))
  }
  csv_fileName = paste(shp,csv_scenario,csv_years,"MonthSum",sep = '_')
  write_csv(monthSum[[i]], file = paste0(results_folder,"/",csv_fileName,".csv",sep = ""))
}


