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

noaa <- read.csv(file = noaa_dataFilepath) # creates dataframe of observed historical data 

noaa$DATE <- mdy(noaa$DATE) # format date using lubridate pkg

noaa <- noaa %>%
  select(STATION, NAME, DATE, PRCP, TMAX, TMIN) %>%
  rename(Station_ID = STATION) %>%
  rename(Station_Name = NAME) %>%
  rename(date = DATE) %>%
  rename(PPT_in = PRCP) %>%
  rename(TMaxF = TMAX) %>%
  rename(TMinF = TMIN) 

df <- noaa # noaa dataframe was altered in NOAA_AllDays script


# -----   ADD DERIVED VARIABLES   -------  #

df <- df %>%
  mutate(Year = year(date)) %>%
  mutate(PPT_mm = RasterUnitConvert(PPT_in, "INtoMM")) %>%
  mutate(TMeanF = (TMaxF + TMinF)/2) %>%
  mutate(GDDF = RasterGDD(TMinF, TMaxF, 50, 86)) %>%
  mutate(TMaxC = RasterUnitConvert(TMaxF, "FtoC")) %>%
  mutate(TMinC = RasterUnitConvert(TMinF, "FtoC"))

df <- df %>%
  mutate(HOTDAYS = Rasterhotdays(TMaxC, hottemp = 32.2)) %>%
  mutate(COLDDAYS = Rastercolddays(TMinC, coldtemp = 0)) %>%
  mutate(WETDAYS = Rasterwetdays(PPT_mm, wetprecip = 50.8)) %>%
  mutate(DRYDAYS = Rasterdrydays(PPT_mm, dryprecip = 2.54)) %>%
  mutate(FTDAYS = RasterFTdays(TMaxC, 
                               TMinC, 
                               freezethresh = -2.2, 
                               thawthresh = 1.2)) %>%
  mutate(VHOTDAYS = fnVHOTDAYS(TMaxC, hottemp = 37.8)) %>%
  mutate(EXHOTDAYS = fnEXHOTDAYS(TMaxC, hottemp = 37.8)) %>%
  mutate(HELLDAYS = fnHELLDAYS(TMaxC, hottemp = 40.6)) %>%
  mutate(WARMNIGHTS = fnWARMNIGHTS(TMinC, coldtemp = 23.9)) %>%
  mutate(FRFRDAYS = fnFRFRDAYS(TMinC, coldtemp = 0)) %>%
  mutate(VWETDAYS = fnVWETDAYS(PPT_mm, wetprecip = 101.6))

df <- select(df,  # remove Celsius values; add new variables for Viewer (2-26-2025)
             Station_ID, 
             Station_Name,
             date,
             Year,
             PPT_in, 
             TMaxF, 
             TMinF, 
             TMeanF, 
             GDDF, 
             HOTDAYS, 
             COLDDAYS, 
             WETDAYS, 
             DRYDAYS, 
             FTDAYS,
             VHOTDAYS,
             EXHOTDAYS,
             HELLDAYS,
             WARMNIGHTS,
             FRFRDAYS,
             VWETDAYS)
           
  

# ---  ASSORT DATA GROUPS BY Year  -----   #

# Sort into groups by Year

grp1 <- df %>%
  filter(Year > 1980 & Year < 2011) 

grp2 <- df %>%
  filter(Year > 1984 & Year < 2015)

grp3 <- df %>%
  filter(Year > 1990 & Year < 2021) 


##  --  CREATE LIST OF DATAFRAMES BY PERIOD --  ##

NOAA_AllDays_Dash <- list()

NOAA_AllDays_Dash[[1]] <- grp1
NOAA_AllDays_Dash[[2]] <- grp2
NOAA_AllDays_Dash[[3]] <- grp3

rm(df)

# ----    CALCULATE 30-Year AVERAGES  ------  #

noaa30 <- list()

for(i in 1:length(NOAA_AllDays_Dash)){

df = NOAA_AllDays_Dash[[i]] 


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


ScenID = i

all = bind_cols("ScenID" = ScenID, "PERIOD" = Period, Pctl90_Prcp_in, sumAvg, Pctl10_Prcp_in, Pctl90_TmaxF, Pctl10_TminF, YearAvg)

all = all %>%
  mutate(across(Pctl90_Prcp_in:TMeanF,  ~ round(., digits = 1)))

noaa30[[i]] = all

}

noaa30dash <- bind_rows(list(noaa30[[1]], noaa30[[2]], noaa30[[3]]))

noaa30dash <- noaa30dash %>%
  mutate(SITENAME = official_name) %>%
  mutate(SCENARIO = "ObsvHist") %>%
  rename(Avg_Prcp_in = PPT_in) %>%
  rename(Avg_TmaxF = TMaxF) %>%
  rename(Avg_TmeanF = TMeanF) %>%
  rename(Avg_TminF = TMinF) 
  

noaa30dash <- select(noaa30dash,
                     SITENAME,
                     SCENARIO,
                     ScenID,
                     PERIOD,
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


rm(list = c("df", "noaa")) 
rm(list = ls(pattern = "^grp"))
rm(list = ls(pattern = "^fileName"))
rm(list = ls(pattern = "^filePath"))










           
           
           
      