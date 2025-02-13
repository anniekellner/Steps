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
    mutate(date = ymd(date)) %>%
    mutate(MonthNum = month(date)) %>%
    mutate(year = year(date)) %>%
    dplyr::select(-c(lat, 
                     lon, 
                     tmax, 
                     tmin, 
                     prcp, 
                     hurs, 
                     huss, 
                     sfcWind,
                     TMaxC,
                     TMinC,
                     TmeanC,
                     PPT_mm
                     ))
  
  yearAvg = df %>%
    dplyr::select(!c('date','PPT_in', 'GDDF')) %>% # exclude variables for which the result is not simply a MonthNumly average
    dplyr::select(!(contains("days"))) %>%
    select(!contains("DAYS")) %>%
    select(!contains("NIGHTS")) %>% 
    group_by(year, MonthNum) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
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
  
  all = yearAvg %>% # Averages by year (e.g., for Jan 1981, Feb 1981...)
    left_join(sum_days) %>%
    left_join(sum_ppt) %>%
    left_join(sum_GDDF) %>%
    left_join(sum_DAYS) %>%
    left_join(sum_nights)
  
  Period = paste(first(all$year),"to",last(all$year), sep = " ")
  
  monthAvg = all %>%
    dplyr::select(!year) %>%
    group_by(MonthNum) %>%
    summarise(across(TMaxF:WARMNIGHTS, ~ mean(.x, na.rm = TRUE))) %>%
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
    left_join(Pctl10_Prcp_in) %>%
    round(digits = 1)
  
 # monthAvg$Period <- Period
  
  #monthAvg$SITENAME <- official_name
  
  monthAvg <- monthAvg %>%
    mutate(Scenario = case_when(
      first(all$year) == 1985 ~ "Modeled Historical Climate",
      first(all$year) == 2021 & i == 2 ~ "Moderate Emissions (SSP2-4.5)",
      first(all$year) == 2051 & i == 3 ~ "Moderate Emissions (SSP2-4.5)",
      first(all$year) == 2021 & i == 4 ~ "High Emissions (SSP5-8.5)",
      first(all$year) == 2051 & i == 5 ~ "High Emissions (SSP5-8.5)")) %>%
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
  
  vars[[i]] = df2
    
}

# --------  ADD TO NOAA MONTHLY DATAFRAME  ---------- #

MonthlySeries <- bind_rows(list(noaaDashboard, vars[[1]], vars[[2]], vars[[3]], vars[[4]], vars[[5]]))


# ----    CALCULATE 30-YEAR AVERAGES  ------  #

avg30 <- MonthlySeries %>%
  select(!MonthNum) %>%
  group_by(SITENAME, Scenario, Period, ScenID) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
  arrange(ScenID) %>%
  ungroup()

avg30 <- avg30 %>%
  mutate(across(where(is.numeric), round, 2))


# --  SAVE SPREADSHEETS  --  #

dash_dir <- paste(results_folder,"Dashboard", sep = "/")

if (!dir.exists(dash_dir)){
  dir.create(dash_dir)}

write.csv(MonthlySeries, file = paste(dash_dir,"MonthlySeries.csv", sep = "/"))
write.csv(avg30, file = paste(dash_dir, "30yr_Averages.csv", sep = "/"))
