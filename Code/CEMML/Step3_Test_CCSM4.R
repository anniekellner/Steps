#--------------------------------
# Name: Step3_LOCA_V2_Gen_ClimateData.R
# Released: 
# Author: Annie Kellner
# Edit Log:
# 
# Description:
#Script runs the initial extraction of data from netcdf files.
#Reads all the RCP model/year combos specified in the script. 
#This script combines three variables (tmax, tmin, precip) 
#into yearly files containing all data 
#for all days. Calls the Function_LOCA_Sum function to add summarize
#and write CSV files at each summary level.
#----------------------------------

# Considerations: 
  # Can make projection alignment more precise by converting all to 4326
  # Can use all pixels touched by base or just the main one(s)

#clear all objects
rm(list=ls())

# Load library packages

library(terra)
library(sf)
library(dplyr)
library(tmap)
library(tmaptools)
library(stringr)
library(data.table)
library(tidyr)

# Load source scripts

source('./Misc/')

# ------------ USER SETTINGS  ---------------------------------- #

## Set directories

dir_installation_boundaries <- "N:\\RStor\\mindyc\\afccm\\AF_CIP_ENV_Data_Phase3/Installation_Boundaries/" # site boundary shape files used for clipping

dir_netcdfs = "N:\\RStor\\mindyc\\afccm\\Climate Modeling\\Data\\LOCA_CCSM4" # netcdfs

dir_output_csvs = "N:\\RStor\\mindyc\\afccm\\Climate Modeling\\Results_LOCA_V2" # output csv's

dir_functions = "N:\\RStor\\mindyc\\afccm\\Climate Modeling\\Software Apps\\R scripts\\LOCA_V2" # scripts that call in functions for analysis

## Select AFB (Air Force Base) - Note: these could be listed in a separate file and pulled in/searched as a vector (using stringi)

AFB_Name = "Homestead_ARB" # I deleted the AFB names from the script because they can be found elsewhere


# -------   LOAD DATA ---------------- #

## NetCDFs
# Could write vector of models into Functions.R
# Write loop to incorporate all scenarios

fileNames <- list.files(dir_netcdfs, pattern = '.nc', full.names = TRUE, recursive = TRUE)

## Select correct files

#Create array
scenario_yr_array = array(1:15, dim=c(5,3))
#colnames(scenario_yr_array) <- c("Scenario", "Year_Start", "Year_End") 

### LOCA only arrays
scenario_yr_array[1,1:3] = c("historical",1976,2005)
scenario_yr_array[2,1:3] = c("rcp45",2026,2035)
scenario_yr_array[3,1:3] = c("rcp45",2046,2055)
scenario_yr_array[4,1:3] = c("rcp85",2026,2035)
scenario_yr_array[5,1:3] = c("rcp85",2046,2055)

# Create historical rasters

hist_filenames_tmax <- fileNames %>%
  str_subset("historical") %>% # time period of interest
  str_subset("tasmax")  # variable of interest

pattern <- paste(seq(scenario_yr_array[1,2], scenario_yr_array[1,3], 1), collapse = "|")  

DT <- data.table(hist_filenames_tmax, result = grepl(pattern, hist_filenames_tmax))  
hist <- DT %>% filter(result == TRUE)

# Remove supplemental files - NOTE: this can be generalized once I get a feel for other likely 'exceptions'

hist_wide <- hist %>% pivot_wider(names_from = hist_filenames_tmax, values_from = result) # flip data frame because 'contains' helper function only works on column names
hist_wide2 <- hist_wide %>%
  select(-contains("supplemental"))

hist_filenames <- colnames(hist_wide2)

# ------------- SPATIAL ANALYSIS ------------------------------------------------ #

## Create multilayered raster

histR <- rast(hist_filenames) # makes a stack of all rasters from hist_filenames list

## Shapefiles

# AFB

afb_dir <- (paste(dir_installation_boundaries, AFB_Name, sep = '/'))
afbSF <- st_read(paste(afb_dir, '.shp', sep = ""))
afbSF <- sf::st_as_sf(afbSF, coords = c("longitude", "latitude"), crs = st_crs(4326))
afb <- vect(afb) # so can play nicely in terra package

###################################################################
##    MIGHT NOT BE NECESSARY  #####################################
###################################################################

# USA

usa <- st_read('./Data/Raw/US_States/Contig_US_Albers.shp') # Reads in in Albers Equal Area
usa <- st_transform(usa, 4326) # Project to LL

# ID state in order to narrow down area for raster processing

getState <- function(AFB){ # Move to functions script 
  afbCentroid = data.frame(longitude = as.numeric(AFB$longitude), latitude =  as.numeric(AFB$latitude))
  afbCentroid = st_as_sf(afbCentroid, coords = c("longitude", "latitude"), crs = 4326)
  int = st_intersection(usa, afbCentroid)
  afb_state = int$STATE_NAME
  return(afb_state)
  }

stateName <- getState(afb)

afb_state <- filter(usa, STATE_NAME == stateName)

rState <- crop(histR, afb_state)

######################################################################
######################################################################

# ------ PLOT TO CHECK THAT LAYERS LINE UP  ------------------------------------ #
# Not working 2/6/23 - need to play around with this

day1 <- histR[[1]] # First layer (1/1/1976)

map <- plot(histR); polys(afb)

# Zoom in for small bases

zoom(day1, e = draw(), layer = 1, new = TRUE)

# ------------ EXTRACT DATA --------------------------------------------------- #

pivot_test <- test %>%
  pivot_longer(cols = starts_with("tasmax"),
               names_to = "Day",
               names_prefix = "tasmax",
               values_to = "Tmax",
               values_drop_na = FALSE)



# Function for extracting multiple values from stack

f <- function(r, na.rm = TRUE){
  c(Tmean = mean(r, na.rm = na.rm),
    Tmin = min(r, na.rm = na.rm),
    Tmax = max(r, na.rm = na.rm))
}

# Next time do system.time()
system.time(
test <- terra::extract(histR, afb, xy = TRUE)
)
#saveRDS(test, file = './Data/Derived/temp.Rds')

histR
afbSF <- st_as_sf(afb)

testVals <- as.data.frame(lat = afbSF$latitude, lon = afbSF$longitude)

system.time(
testVals <- values() 
)

