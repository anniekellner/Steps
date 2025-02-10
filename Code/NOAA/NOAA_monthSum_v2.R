###################################################################
###             OBSERVED HISTORICAL DATA:                       ### 
###                 MONTHLY SUMMARIES                           ###
###               ORIGINAL OUTPUT (CSV FILES AND DATAFRAME)     ###
###################################################################

# Written/updated by Annie Kellner (annie.kellner@colostate.edu)
# 2-9-2025

# Inputs: AllDays_hist dataframe (concatenated NOAA daily data)

# Outputs: 
    # .csv files (3): monthly summaries for 3 historical time periods
    # monthSum dataframe


### BEGIN SCRIPT

noaa_monthSum <- list()

for(i in 1:length(AllDays_hist)){
  
  df = AllDays_hist[[i]]
 
   df = df %>%
    mutate(month = month(date)) %>%
    mutate(year = year(date)) 
   
   yearAvg = df %>%
     dplyr::select(!c('date','PPT_in', 'PPT_mm', 'GDDF')) %>% # exclude variables for which the result is not simply a monthly average
     dplyr::select(!(contains("days"))) %>%
     group_by(year, month) %>%
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
   
   sum_days = df %>%
     select(date, year, MonthNum, contains('days')) %>%
     group_by(year, MonthNum) %>%
     summarise(across(contains('days'), ~ sum(.x, na.rm = TRUE))) %>%
     ungroup()
   
   sum_GDDF = df %>%
     select(date, year, MonthNum, GDDF) %>%
     group_by(year, MonthNum) %>%
     summarise(GDDF = sum(GDDF, na.rm = TRUE)) %>%
     ungroup()
   
   all = yearAvg %>% # Averages by year (e.g., for Jan 1981, Feb 1981...)
     left_join(sum_days) %>%
     left_join(sum_ppt) %>%
     left_join(Abs_TminF) %>%
     left_join(GDDF)
   
   monthAvg = all %>%
     dplyr::select(!year) %>%
     group_by(month) %>%
     summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
     round(digits = 1) %>%
     setNames(paste0('Avg_', names(.))) %>%
     rename(Abs_TminF = Avg_Abs_TminF) %>%
     rename(month = Avg_month)
     select(month, # put in order on MonthSum csv
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
            Avg_ftdays
     )
     
     noaa_monthSum[[i]] <- monthAvg
}
   
