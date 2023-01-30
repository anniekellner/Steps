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

#clear all objects
rm(list=ls())

# Load library packages

library(stars) # loads sf
library(dplyr)
library(tmap)
library(tmaptools)

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

#Search buffer (miles) that is used to expand search for points. 
search_buff = 1  # Change search buffer in 0.5 mi increments as needed for small sites. 
                 # For large sites can change to zero to improve performance.

###End of Run parameters
#####################################

# -------   LOAD DATA ---------------- #

## NetCDF

r <- rast('./Data/Raw/tasmax_day_CCSM4_historical_r6i1p1_19760101-19761231.LOCA_2016-04-02.16th.nc')

st <- read_stars('./Data/Raw/tasmax_day_CCSM4_historical_r6i1p1_19760101-19761231.LOCA_2016-04-02.16th.nc') # 1976. Daily data.
st <- st_as_stars(st) # converts nc to stars object 
st <- setNames(st, "tmax") # set attribute name to tmax (because raw data is in Kelvin) 

## Shapefiles

afb_dir <- (paste(dir_installation_boundaries, AFB_Name, sep = '/'))
afb <- st_read(paste(afb_dir, '.shp', sep = ""))

# ------- PREP DATA ----------------------- #

## Projections

st <- st_set_crs(st, 4326)
afb2 <- st_transform(afb, 4326)

## Quick interactive plot to make sure things are lined up 
tmap_mode('view')

tm_shape(day100) + 
  tm_raster(col = day100$tmax) + 
  tm_shape(afb2) + 
  tm_borders()

# NOTE: some AFB's are so small they only span one cell. For these it is better to select cell index. 
# Maybe 

# add something to check that crs(x) = crs(y)

## Crop NetCDF to base

stAFB <- st_crop(st, afb2, crop = TRUE, as_points = TRUE)

## Plot first day to spot check

day100 <- slice(st, along = "time", 100)
day2 <- slice(st, along = "time", 2)

ras <- rast(day100)
library(terra)

ras2 <- crop(ras, afb2)
