########################################################
##    HISTORICAL CLIMATE DATA: ALL DAYS SUMMARY       ##
########################################################

# written by Annie Kellner for CEMML 12-10-2024
# annie.kellner@colostate.edu

# Add derived (recalculated) variables to dataframes
# Functions and hard-coded values are from 
    # the original LOCA_summarize.R script and
    # new functions adapted from that script (2-8-2025)


## BEGIN SCRIPT

df <- noaa # noaa dataframe was altered in NOAA_AllDays script


# -----   ADD DERIVED VARIABLES   -------  #

df <- df %>%
  mutate(month = month(date)) %>%
  mutate(year = year(date)) %>%
  mutate(PPT_mm = RasterUnitConvert(PPT_in, "INtoMM")) %>%
  mutate(TMeanF = (TMaxF + TMinF)/2) %>%
  mutate(GDDF = RasterGDD(TMinF, TMaxF, 50, 86)) %>%
  mutate(TMaxC = RasterUnitConvert(TMaxF, "FtoC")) %>%
  mutate(TMinC = RasterUnitConvert(TMinF, "FtoC"))

df <- df %>%
  mutate(hotdays = Rasterhotdays(TMaxC, hottemp = 32.2)) %>%
  mutate(colddays = Rastercolddays(TMinC, coldtemp = 0)) %>%
  mutate(wetdays = Rasterwetdays(PPT_mm, wetprecip = 50.8)) %>%
  mutate(drydays = Rasterdrydays(PPT_mm, dryprecip = 2.54)) %>%
  mutate(ftdays = RasterFTdays(TMaxC, 
                               TMinC, 
                               freezethresh = -2.2, 
                               thawthresh = 1.2)) %>%
  mutate(VHOTDAYS = fnVHOTDAYS(TMaxC, hottemp = 37.8)) %>%
  mutate(EXHOTDAYS = fnEXHOTDAYS(TMaxC, hottemp = 37.8)) %>%
  mutate(HELLDAYS = fnHELLDAYS(TMaxC, hottemp = 40.6)) %>%
  mutate(WARMNIGHTS = fnWARMNIGHTS(TMinC, coldtemp = 23.9)) %>%
  mutate(FRFRDAYS = fnFRFRDAYS(TMinC, coldtemp = 0)) %>%
  mutate(VWETDAYS = fnVWETDAYS(PPT_mm, wetprecip = 101.6))
           
  
df <- select(df,  # remove metric units
             year,
             Station_ID, 
             Station_Name,
             date,
             PPT_in, 
             TMaxF, 
             TMinF, 
             TMeanF, 
             GDDF, 
             hotdays, 
             colddays, 
             wetdays, 
             drydays, 
             ftdays,
             VHOTDAYS,
             EXHOTDAYS,
             HELLDAYS,
             WARMNIGHTS, 
             FRFRDAYS,
             VWETDAYS)

# ---  ASSORT DATA GROUPS BY YEAR AND SAVE   -----   #

# Sort into groups by year

grp1 <- df %>%
  filter(year > 1980 & year < 2011) %>%
  select(-year) 

grp2 <- df %>%
  filter(year > 1984 & year < 2015) %>%
  select(-year)

grp3 <- df %>%
  filter(year > 1990 & year < 2021) %>%
  select(-year)

##  --  CREATE LIST OF DATAFRAMES BY PERIOD --  ##

NOAA_AllDays_Dash <- list()

NOAA_AllDays_Dash[[1]] <- grp1
NOAA_AllDays_Dash[[2]] <- grp2
NOAA_AllDays_Dash[[3]] <- grp3


# ----    CALCULATE 30-YEAR AVERAGES  ------  #

df = grp1 %>%
  mutate(Year = year(date))

Pctl90_TmaxF = df %>%
  select(TMaxF) %>%
  summarize(Pctl90_TmaxF = quantile(TMaxF, probs = 0.90, na.rm = TRUE)) %>%
  ungroup()

Pctl10_TminF = df %>%
  select(TMinF) %>%
  summarize(Pctl10_TminF = quantile(TMinF, probs = 0.10, na.rm = TRUE)) %>%
  ungroup()

yearAvg = df %>%
  dplyr::select(!c('date', 'Year', 'PPT_in', 'GDDF')) %>% # exclude variables for which the result should be summed before averaging
  dplyr::select(!(contains("days"))) %>%
  select(!contains("DAYS")) %>%
  select(!contains("NIGHTS")) %>% 
  group_by(year, MonthNum) %>%
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

monthAvg = monthAvg %>%
  left_join(Pctl90_TmaxF) %>%
  left_join(Pctl10_TminF) %>%
  left_join(Pctl90_Prcp_in) %>%
  left_join(Pctl10_Prcp_in) %>%
  round(digits = 1)

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





sums = sum_days %>% # Averages by year (e.g., 1981, 1982...2010)
  left_join(sum_ppt) %>%
  left_join(sum_GDDF) %>%
  left_join(sum_DAYS) %>%
  left_join(sum_nights)

Period = paste(first(yearAvg$Year),"to",last(yearAvg$Year), sep = " ")

sumAvg = sums %>%
  dplyr::select(!Year) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  ungroup()



avg30_grp1 <- grp1 %>%
  select(!c(Station_ID, Station_Name)) %>%
  






rm(list = c("df", "noaa")) 
rm(list = ls(pattern = "^grp"))
rm(list = ls(pattern = "^fileName"))
rm(list = ls(pattern = "^filePath"))









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


           
           
           
      