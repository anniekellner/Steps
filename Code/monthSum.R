###############################################################################
###         MONTHLY SUMMARY OF VARIABLES                                    ###
###############################################################################


# This script produces the following outputs:
  # monthSum.csv for traditional use with written reports
  # Monthly Summary formatted for Climate Viewer input
  # monthSum dataframe for use in downstream analyses

# Inputs: 
  # AllDays dataframe (tibble)



# amended by Annie Kellner (annie.kellner@colostate.edu) 2-6-2025


monthSum <- list()

MACA = if_else(str_detect(model, "MACA"), # this code is not yet functional, but does not disrupt the process
               TRUE,
               FALSE)

for(i in 1:length(AllDays)){
  df = AllDays[[i]]
  df = df %>%
    mutate(date = ymd(date)) %>%
    mutate(month = month(date)) %>%
    mutate(year = year(date)) %>%
    dplyr::select(-c(lat, lon))
  
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
  
  # As above, MACA-related code is a placeholder for future integration
  
  macaCols = if(MACA == "TRUE") {getMACAcols(all)} else {NULL} 
  
  monthAvg = if(MACA == "TRUE") {bind_cols(monthAvg, macaCols)} else {monthAvg} 
  
  # End of MACA code
  
  monthSum[[i]] = monthAvg # new df for export and use with DiffHist
  names(monthSum)[i] = names(AllDays[i])
}

# Add summary rows (YrAverage and YrTotals) and save for table construction

Avs_and_Totals <- list() # saving for summary table

for(i in 1:length(monthSum)){
  YrAverage = colMeans(monthSum[[i]][,2:ncol(monthSum[[i]])]) # converts columns to characters 
  YrTotals = colSums(monthSum[[i]][,2:ncol(monthSum[[i]])])
  csv = bind_rows(monthSum[[i]], YrAverage, YrTotals)
  
  # NA's 
  
  NAs = csv %>% # YrAverage
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
  
  NAs_Totals = if(MACA == "TRUE") {mutate(across(.cols = .cols %in% colnames(macaCols), ~na_if(.,.)))
  } else {NAs_Totals}
  
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


