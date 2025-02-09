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


##  --  PREPARE FOR MONTHLY SUMMARY --  ##

AllDays_Dash <- list()

AllDays_Dash[[1]] <- grp1
AllDays_Dash[[2]] <- grp2
AllDays_Dash[[3]] <- grp3

rm(c("df", "noaa")) 
rm(list = ls(pattern = "^grp"))
rm(list = ls(pattern = "^fileName"))
rm(list = ls(pattern = "^filePath"))





           
           
           
      