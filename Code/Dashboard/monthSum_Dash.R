###############################################################################
###           CREATE MONTHLY SERIES SPREADSHEET FOR DASHBOARD              ###
###############################################################################

# written by Annie Kellner (annie.kellner@colostate.edu)
# 2-10-2025

# This script generates the Modeled Monthly portion of the Dashboard spreadsheets


## BEGIN SCRIPT

vars <- list()

for(i in 1:length(AllDaysDash)){
  df = AllDaysDash[[i]]
  
  df = df %>%
    mutate(date = date(date)) %>%
    mutate(date = ymd(date)) %>%
    mutate(MonthNum = month(date)) 
  
  yearAvg = df %>%
    dplyr::select(!c('date','PPT_in', 'GDDF')) %>% # exclude variables for which the result is not simply a Monthly average
    select(!contains("DAYS")) %>%
    select(!contains("NIGHTS")) %>% 
    group_by(Year, MonthNum) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop_last") %>%
    ungroup()

  sum_ppt = df %>%
    select(date, Year, MonthNum, 'PPT_in') %>%
    group_by(Year, MonthNum) %>%
    summarise(across(contains('PPT'), ~ sum(.x, na.rm = TRUE)), .groups = "drop_last") %>%
    ungroup()
  
  sum_DAYS = df %>%
    select(date, Year, MonthNum, contains('DAYS')) %>%
    group_by(Year, MonthNum) %>%
    summarise(across(contains('DAYS'), ~ sum(.x, na.rm = TRUE)), .groups = "drop_last") %>%
    ungroup()
  
  sum_nights = df %>%
    select(date, Year, MonthNum, contains('NIGHTS')) %>%
    group_by(Year, MonthNum) %>%
    summarise(across(contains('nights'), ~ sum(.x, na.rm = TRUE)), .groups = "drop_last") %>%
    ungroup()
  
  sum_GDDF = df %>%
    select(date, Year, MonthNum, GDDF) %>%
    group_by(Year, MonthNum) %>%
    summarise(GDDF = sum(GDDF, na.rm = TRUE), .groups = "drop_last") %>%
    ungroup()
  
  all = yearAvg %>% # Averages by year (e.g., for Jan 1981, Feb 1981...)
    left_join(sum_ppt) %>%
    left_join(sum_GDDF) %>%
    left_join(sum_DAYS) %>%
    left_join(sum_nights)
  
  Period = paste(first(all$Year),"to",last(all$Year), sep = " ")
  
  monthAvg = all %>%
    dplyr::select(!Year) %>%
    group_by(MonthNum) %>%
    summarise(across(TMaxF:WARMNIGHTS, ~ mean(.x, na.rm = TRUE)), .groups = "drop_last") %>%
    ungroup()
  
  monthAvg = monthAvg %>%
    setNames(paste0('Avg_', names(.))) %>%
    rename(MonthNum = Avg_MonthNum) %>%
    rename(Avg_Prcp_in = Avg_PPT_in) %>%
    rename(Avg_TmaxF = Avg_TMaxF) %>%
    rename(Avg_TmeanF = Avg_TMeanF) %>%
    rename(Avg_TminF = Avg_TMinF) %>%
    rename(HOTDAYS_90to95 = Avg_HOTDAYS_90to95) %>%
    rename(HOTDAYS_95to100 = Avg_HOTDAYS_95to100) %>%
    rename(HOTDAYS_100to105 = Avg_HOTDAYS_100to105) %>%
    rename(HOTDAYS_105plus = Avg_HOTDAYS_105plus) %>%
    rename(WARMNIGHTS = Avg_WARMNIGHTS) %>%
    rename(COLDDAYS = Avg_COLDDAYS) %>%
    rename(FRFRDAYS = Avg_FRFRDAYS) %>%
    rename(FTDAYS = Avg_FTDAYS) %>%
    rename(GDDF = Avg_GDDF) %>%
    rename(DRYDAYS = Avg_DRYDAYS) %>%
    rename(WETDAYS = Avg_WETDAYS) %>%
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
    left_join(Pctl10_Prcp_in) %>%
    round(digits = 1)
  
  monthAvg <- monthAvg %>%
    mutate(Scenario = case_when(
      first(all$Year) == 1985 ~ "Modeled Historical Climate",
      first(all$Year) == 2021 & i == 2 ~ "Moderate Emissions (SSP2-4.5)",
      first(all$Year) == 2051 & i == 3 ~ "Moderate Emissions (SSP2-4.5)",
      first(all$Year) == 2021 & i == 4 ~ "High Emissions (SSP5-8.5)",
      first(all$Year) == 2051 & i == 5 ~ "High Emissions (SSP5-8.5)")) %>%
    mutate(Period = Period) %>%
    mutate(SITENAME = official_name)
  
  monthAvg$Month <- month.abb[monthAvg$MonthNum]
  
  vars[[i]] = monthAvg 
  
}

rm(df)

# Add Scenario ID (hard-coded for now)

vars[[1]]$ScenID <- "4"
vars[[2]]$ScenID <- "5"
vars[[3]]$ScenID <- "6"
vars[[4]]$ScenID <- "7"
vars[[5]]$ScenID <- "8"

# Order columns for spreadsheet

for(i in 1:length(vars)){
  
  df = vars[[i]]
  df2 = df %>% 
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
           HOTDAYS_90to95,
           HOTDAYS_95to100,
           HOTDAYS_100to105,
           HOTDAYS_105plus,
           WARMNIGHTS,
           COLDDAYS,
           FRFRDAYS,
           FTDAYS,
           GDDF,
           DRYDAYS,
           WETDAYS,
           VWETDAYS)
  
  vars[[i]] = df2
    
}

# --------  ADD TO NOAA MONTHLY DATAFRAME  ---------- #

MonthlySeries <- bind_rows(list(noaaDashboard, 
                                vars[[1]], vars[[2]], vars[[3]], vars[[4]], vars[[5]]))


# ------  SAVE SPREADSHEET   ----------- #

filename <- paste(shp, "MonthlySeries.csv", sep = "_")

write.csv(MonthlySeries, file = paste(dash_dir,filename,sep = "/"), row.names = FALSE)



