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

# ------------ USER SETTINGS  ---------------------------------- #

## Set directories

dir_installation_boundaries <- "N:\\RStor\\mindyc\\afccm\\AF_CIP_ENV_Data_Phase3/Installation_Boundaries/" # site boundary shape files used for clipping

dir_netcdfs = "N:\\RStor\\mindyc\\afccm\\Climate Modeling\\Data\\LOCA_CCSM4" # netcdfs

dir_output_csvs = "N:\\RStor\\mindyc\\afccm\\Climate Modeling\\Results_LOCA_V2" # output csv's

dir_functions = "N:\\RStor\\mindyc\\afccm\\Climate Modeling\\Software Apps\\R scripts\\LOCA_V2" # scripts that call in functions for analysis

## Select AFB (Air Force Base) - Note: these could be listed in a separate file and pulled in/searched as a vector (using stringi)

AFB_Name = "Homestead_ARB"

#AFB_Name = "Hanscom_FourthCliff"
#AFB_Name = "JBSA_SAF"
#AFB_Name = "JBSA_RND"
#AFB_Name = "JBSA_MED-LAK-KFA-PRT"
#AFB_Name = "JBSA_GSA-MCA-SAM"
#AFB_Name = "JBSA_CAN"
#AFB_Name = "JBSA_BUL"
#AFB_Name = "Dobbins_AFB"
#AFB_Name = "LakeYellowstone"
#AFB_Name = "Laughlin"
#AFB_Name = "Dover_AFB"
#AFB_Name = "Hanscom_wGSUs"
#AFB_Name = "JBAB_Proxy"
#AFB_Name = "RomeLab_Main"
#AFB_Name = "Rome_Verona"
#AFB_Name = "Badlands_Range"
#AFB_Name = "Eglin_Niceville"
#AFB_Name = "Cape_Cod_AS"
#AFB_Name = "Fairchild_AFB"
#AFB_Name = "PointArena_AFS_Proxy"
#AFB_Name = "Ellsworth_AFB"
#AFB_Name = "FEWarren_AFB"
#AFB_Name = "Buckley_SFB_Dissolved"
#AFB_Name = "Cheyenne_Mountain_HighElevationProxyv2_Sugarloaf"
#AFB_Name = "Offut_Communications_Annex_#3_Boundary"
#AFB_Name = "Offutt_AFB_Boundary"
#AFB_Name = "Minot_AFB_Boundary"
#AFB_Name = "GFAFB_Waste_Lagoon_Annex_Boundary"
#AFB_Name = "GFAFB_Boundary"
#AFB_Name = "WPAFB_Boundary"
#AFB_Name = "Scott_AFB_Boundary"
#AFB_Name = "MAFB_Boundary"
# AFB_Name = "MHAFB_Boundary"
# AFB_Name = "Malmstrom_Deployment_Area"
# AFB_Name = "US_Air_Force_Academy"
# AFB_Name = "Peterson_AFB"
# AFB_Name = "USAF_Academy_Farish_Buffer_1mi"
# AFB_Name = "USAF_Academy_Bullseye_Buffer_1mi"
# AFB_Name = "Schriever_AFB"
# AFB_Name = "Malmstrom_AFB_Boundary"


###End of Run parameters
#####################################



# -------   LOAD DATA ---------------- #

## NetCDFs
# Could write vector of models into Functions.R
# Write loop to incorporate all scenarios

fileNames <- list.files(dir_netcdfs, pattern = '.nc', full.names = TRUE, recursive = TRUE)

## Select correct files

#Create array
scenario_yr_array = array(1:15, dim=c(5,3))
colnames(scenario_yr_array) <- c("Scenario", "Year_Start", "Year_End") # unnecessary

### LOCA only arrays
scenario_yr_array[1,1:3] = c("historical",1976,2005)
scenario_yr_array[2,1:3] = c("rcp45",2026,2035)
scenario_yr_array[3,1:3] = c("rcp45",2046,2055)
scenario_yr_array[4,1:3] = c("rcp85",2026,2035)
scenario_yr_array[5,1:3] = c("rcp85",2046,2055)

# Create historical raster

hist_files <- fileNames %>%
  str_subset("historical") %>% # time period of interest
  str_subset("tasmax")  # variable of interest

pattern <- paste(seq(scenario_yr_array[1,2], scenario_yr_array[1,3], 1), collapse = "|")  

DT <- data.table(hist_files, result = grepl(pattern, hist_files))  
hist <- DT %>% filter(result == TRUE)

# Remove supplemental files - NOTE: this can be generalized once I get a feel for other likely 'exceptions'

hist_wide <- hist %>% pivot_wider(names_from = hist_files, values_from = result) # flip data frame because 'contains' helper function only works on column names
hist_wide2 <- hist_wide %>%
  select(-contains("supplemental"))

hist_filenames <- colnames(hist_wide2)






#r <- rast('./Data/Raw/tasmax_day_CCSM4_historical_r6i1p1_19760101-19761231.LOCA_2016-04-02.16th.nc') # Trial run with 1976


## Shapefiles

# AFB

afb_dir <- (paste(dir_installation_boundaries, AFB_Name, sep = '/'))
afb <- st_read(paste(afb_dir, '.shp', sep = ""))

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

rState <- crop(r, afb_state)

# ------ TEST  ------------------------------------ #

day1 <- r[[1]] # First layer (1/1/1976)

rState2 <- crop(day1, afb_state) # Crop first layer to state of FL

rState3 <- project(rState2, "EPSG:4326")


# Quick interactive plot to make sure AFB is where it's supposed to be
# Not much of a diff

tmap_mode('view')

# No meaningful difference between projecting both to 4326 and leaving as is in WGS84
tm_shape(rState3) + 
  tm_raster() +
  tm_shape(afb) + 
  tm_borders()

# ------- PREP DATA ----------------------- #

## Crop raster to afb

rAFB <- crop(rState2, afb) # can change snap setting to get all pixels touched by AFB if desired

# Plot check

tm_shape(rAFB) +
  tm_raster() +
  tm_shape(afb) + 
  tm_borders()

# Deciding to just take the main pixel and see how it goes




