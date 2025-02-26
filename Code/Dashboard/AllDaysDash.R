###############################################################################
###           ALLDAYS DATAFRAME FOR CLIMATE VIEWER DASHBOARD                ###
###############################################################################

# written by Annie Kellner (annie.kellner@colostate.edu)

# Derives daily values for Climate Viewer variables
# Functions from and adapted from original LOCA_summarize.R script 
# Adapted 2-9-2025 

# Added Vapor Pressure Deficit (calculated from hurs) and
# Specific Humidity (huss; extracted directly from model)

# Inputs: 
# avdf list ("all values data frame")

# Outputs: 
# AllDays_Dash dataframe for downstream use

## BEGIN SCRIPT

AllDaysDash <- list() # required for monthSumDash


for(i in 1:length(avdf)){
  csv = avdf[[i]]
  csv = csv %>%
    mutate(PPT_mm = case_when( # if raster units are "kg-m-2 -1", convert to mm (otherwise keep as is)
      str_detect(units(rx), "kg") ~ prcp*86400,
      TRUE ~ as.numeric(prcp))) %>%
    mutate(PPT_in = RasterUnitConvert(PPT_mm, "MMtoIN")) %>%
    mutate(TMaxF = case_when(
      tmax > 200 ~ RasterUnitConvert(tmax, "KtoF"), # Because only values in Kelvin would be > 200
      TRUE ~ RasterUnitConvert(tmax, "CtoF"))) %>% # assuming if temp units are not Kelvin they are Celsius
    mutate(TMaxC = case_when(
      tmax > 200 ~ RasterUnitConvert(tmax, "KtoC"),
      TRUE ~ tmax)) %>% # assuming if temp units are not Kelvin they are Celsius
    mutate(TMinF = case_when(
      tmin > 200 ~ RasterUnitConvert(tmin, "KtoF"),
      TRUE ~ RasterUnitConvert(tmin, "CtoF"))) %>% # assuming if temp units are not Kelvin they are Celsius
    mutate(TMinC = case_when(
      tmin > 200 ~ RasterUnitConvert(tmin, "KtoC"),
      TRUE ~ tmin)) %>% # assuming if temp are not K they are C
    mutate(TMeanF = (TMaxF + TMinF)/2) %>%
    mutate(TmeanC = (TMaxC + TMinC)/2) %>%
    mutate(GDDF = RasterGDD(TMinF, TMaxF, 50, 86)) %>% # All hard-coded values are from the original CEMML script
    mutate(hotdays = Rasterhotdays(TMaxC, hottemp = 32.2)) %>% # Hard-coded values may be changed at user discretion
    mutate(colddays = Rastercolddays(TMinC, coldtemp = 0)) %>%
    mutate(wetdays = Rasterwetdays(PPT_mm, wetprecip = 50.8)) %>%
    mutate(drydays = Rasterdrydays(PPT_mm, dryprecip = 2.54)) %>%
    mutate(ftdays = RasterFTdays(TMaxC, TMinC, freezethresh = -2.2, thawthresh = 1.2)) %>%
    mutate(VHOTDAYS = fnVHOTDAYS(TMaxC, hottemp = 37.8)) %>%
    mutate(EXHOTDAYS = fnEXHOTDAYS(TMaxC, hottemp = 37.8)) %>%
    mutate(HELLDAYS = fnHELLDAYS(TMaxC, hottemp = 40.6)) %>%
    mutate(WARMNIGHTS = fnWARMNIGHTS(TMinC, coldtemp = 23.9)) %>%
    mutate(FRFRDAYS = fnFRFRDAYS(TMinC, coldtemp = 0)) %>%
    mutate(VWETDAYS = fnVWETDAYS(PPT_mm, wetprecip = 101.6))
  
  AllDaysDash[[i]] = csv # new df for use with MonthSum
  names(AllDaysDash)[i] = names(avdf[i])
  
} # end creation of AllDaysDash dataframe


# ----    CALCULATE 30-YEAR AVERAGES  ------  #

alldays30 <- for(i in 1:length(AllDaysDash)){
  
  df = AllDaysDash[[i]] 
  
  Period = paste(first(df$Year),"to",last(df$Year), sep = " ")
  
  Pctl90_TmaxF = df %>%
    select(TMaxF) %>%
    summarize(Pctl90_TmaxF = quantile(TMaxF, probs = 0.90, na.rm = TRUE)) %>%
    ungroup()
  
  Pctl10_TminF = df %>%
    select(TMinF) %>%
    summarize(Pctl10_TminF = quantile(TMinF, probs = 0.10, na.rm = TRUE)) %>%
    ungroup()
  
  YearAvg = df %>%
    dplyr::select(!c('date', 'Year', 'PPT_in', 'GDDF')) %>% # exclude variables for which the result should be summed before averaging
    dplyr::select(!(contains("days"))) %>%
    select(!contains("DAYS")) %>%
    select(!contains("NIGHTS")) %>% 
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
    ungroup()
  
  sum_ppt = df %>%
    select(Year, 'PPT_in') %>%
    group_by(Year) %>%
    summarise(across(contains('PPT'), ~ sum(.x, na.rm = TRUE))) %>%
    ungroup()
  
  Pctl90_Prcp_in = sum_ppt %>%
    summarize(Pctl90_Prcp_in = quantile(PPT_in, probs = 0.90, na.rm = TRUE)) %>%
    ungroup()
  
  Pctl10_Prcp_in = sum_ppt %>%
    summarize(Pctl10_Prcp_in = quantile(PPT_in, probs = 0.10, na.rm = TRUE)) %>%
    ungroup()
  
  
  sum_days = df %>%
    select(Year, contains('days')) %>%
    group_by(Year) %>%
    summarise(across(contains('days'), ~ sum(.x, na.rm = TRUE))) %>%
    ungroup()
  
  sum_DAYS = df %>%
    select(Year, contains('DAYS')) %>%
    group_by(Year) %>%
    summarise(across(contains('DAYS'), ~ sum(.x, na.rm = TRUE))) %>%
    ungroup()
  
  sum_nights = df %>%
    select(Year, contains('NIGHTS')) %>%
    group_by(Year) %>%
    summarise(across(contains('nights'), ~ sum(.x, na.rm = TRUE))) %>%
    ungroup()
  
  sum_GDDF = df %>%
    select(Year, GDDF) %>%
    group_by(Year) %>%
    summarise(GDDF = sum(GDDF, na.rm = TRUE)) %>%
    ungroup()
  
  sums = sum_days %>% # Averages by Year (e.g., 1981, 1982...2010)
    left_join(sum_ppt) %>%
    left_join(sum_GDDF) %>%
    left_join(sum_DAYS) %>%
    left_join(sum_nights)
  
  sumAvg = sums %>%
    dplyr::select(!Year) %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
    ungroup()
  
  
  ScenID = i + 3
  
  all = bind_cols("ScenID" = ScenID, "PERIOD" = Period, Pctl90_Prcp_in, sumAvg, Pctl10_Prcp_in, Pctl90_TmaxF, Pctl10_TminF, YearAvg)
  
  all = all %>%
    mutate(across(Pctl90_Prcp_in:TMeanF,  ~ round(., digits = 1)))
  
  alldays30[[i]] = all
  
}

# --  SAVE SPREADSHEETS  --  #

dash_dir <- paste(results_folder,"Dashboard", sep = "/")

if (!dir.exists(dash_dir)){
  dir.create(dash_dir)}

write.csv(MonthlySeries, file = paste(dash_dir,"MonthlySeries.csv", sep = "/"))
write.csv(avg30, file = paste(dash_dir, "30yr_Averages.csv", sep = "/"))