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

#### EXAMPLE FROM MONTHSUM  #######

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