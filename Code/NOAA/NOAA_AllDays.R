########################################################
##    HISTORICAL CLIMATE DATA: ALL DAYS SUMMARY       ##
########################################################

# written by Annie Kellner for CEMML 12-10-2024
# annie.kellner@colostate.edu

# Add derived (recalculated) variables to dataframes
# All functions and hard-coded values are from 
    # the original LOCA_summarize.R script


# ----- PREP DATA ------------------  #

noaa$DATE <- mdy(noaa$DATE) # format date using lubridate pkg

noaa <- noaa %>%
  select(STATION, NAME, DATE, PRCP, TMAX, TMIN) %>%
  rename(Station_ID = STATION) %>%
  rename(Station_Name = NAME) %>%
  rename(date = DATE) %>%
  rename(PPT_in = PRCP) %>%
  rename(TMaxF = TMAX) %>%
  rename(TMinF = TMIN) 

df <- noaa # DELETE ONCE SCRIPT IS SET


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
                               thawthresh = 1.2)) 

df <- select(df,  # remove Celsius temps
             year,
             Station_ID, 
             Station_Name,
             date,
             PPT_in, 
             PPT_mm, 
             TMaxF, 
             TMinF, 
             TMeanF, 
             GDDF, 
             hotdays, 
             colddays, 
             wetdays, 
             drydays, 
             ftdays)

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

# Save .csv's to Results folder

# Group 1

fileName_grp1 <- paste(weather_station,"1981-2010","historical","AllDays", sep = "_")
filePath_grp1 <- paste0(noaa_resultsDir,"/",fileName_grp1,".csv")

write.csv(grp1, file = filePath_grp1)

# Group 2

fileName_grp2 <- paste(weather_station,"1985-2014","historical","AllDays", sep = "_")
filePath_grp2 <- paste0(noaa_resultsDir,"/",fileName_grp2,".csv")

write.csv(grp2, file = filePath_grp2)

# Group 3

fileName_grp3 <- paste(weather_station,"1991-2020","historical","AllDays", sep = "_")
filePath_grp3 <- paste0(noaa_resultsDir,"/",fileName_grp3,".csv")

write.csv(grp3, file = filePath_grp3)

##  --  PREPARE FOR MONTHLY SUMMARY --  ##

AllDays_hist <- list()

AllDays_hist[[1]] <- grp1
AllDays_hist[[2]] <- grp2
AllDays_hist[[3]] <- grp3

rm(list = c("df","noaa")) 
rm(list = ls(pattern = "^grp"))
rm(list = ls(pattern = "^fileName"))
rm(list = ls(pattern = "^filePath"))





           
           
           
      