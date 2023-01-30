#--------------------------------
# Name: LOCA_V2_Gen_ClimateData.R
# Released: 6/18/2020
# Author: Bob Flynn
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

loca <- read_stars('N:/RStor/mindyc/afccm/Climate Modeling/Data/LOCA_CCSM4/rcp45/tasmax_day_CCSM4_rcp45_r6i1p1_20060101-20061231.LOCA_2016-04-02.16th.nc')

had <- read_stars('N:/RStor/mindyc/afccm/Climate Modeling/Data/HADGEM2-ES/rcp45/tasmax_day_HadGEM2-ES_rcp45_r1i1p1_EWEMBI_20060101-20101231.nc4')

#Display start time
cat("\n",paste("Start time is ", Sys.time()),"\n")

#clear all objects
rm(list=ls())

#First - - Load all Library packages
library(ncdf4)
require(sp)
require(rgdal)
require(maps)
library(chron)
library (geosphere)
require (sqldf)
library (rgeos)
library(stars)
library(dplyr)

#########################################
# #Run parameters - MODIFY AS Needed 

#Set variable for boundary clip file name

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


#Search buffer (miles) that is used to expand search for points. Change search buffer in 0.5 mi increments
#    as needed for small sites. For large sites can change to zero to improve performance.
search_buff = 1

#Create array
scenario_yr_array = array(1:15, dim=c(5,3))

# scenario_yr_array = c(rep(c("","",""),5))

### LOCA only arrays
scenario_yr_array[1,1:3] = c("historical",1976,2005)
scenario_yr_array[2,1:3] = c("rcp45",2026,2035)
scenario_yr_array[3,1:3] = c("rcp45",2046,2055)
scenario_yr_array[4,1:3] = c("rcp85",2026,2035)
scenario_yr_array[5,1:3] = c("rcp85",2046,2055)


#Set variable for directory for site boundary shape files used for clipping
#wdir_clipbound = "N:\\RStor\\mindyc\\compass\\INRMP_CC\\Nellis AFB\\Climate Projections/Spatial Data/WGS84/"
wdir_clipbound = "N:\\RStor\\mindyc\\afccm\\AF_CIP_ENV_Data_Phase3/Installation_Boundaries/"

#Set variable for the working directory for NetCDF files
wdir_netcdf = "N:\\RStor\\mindyc\\afccm\\Climate Modeling\\Data\\HADGEM2-ES"

#Set variable for the working directory for Output CSV files
#wdir_outputcsv = "N:\\RStor\\mindyc\\compass/INRMP_CC/Nellis AFB/Climate Projections/LOCA CCSM4 Climate Data/"
wdir_outputcsv = "N:\\RStor\\mindyc\\afccm\\Climate Modeling\\Results_HADGEM2-ES"

#Set variable for directory where function scripts are located
wdir_functions = "N:\\RStor\\mindyc\\afccm\\Climate Modeling\\Software Apps\\R scripts\\LOCA_V2"

#Set Clip type. Default is Polygon and will change to PointCentroid automatically if area is too small.
# -- NOTE - Only change this for one-time overide run when polygon clip fails and you know what you are doing. 
# Change back to Polygon when done
clip_type = "Polygon"
# clip_type = "PointCentroid"
# clip_type = "Point"  
#Point clip requires lon/lat values below
#    stn_lon = -104.22
#    stn_lat = 43.44



###End of Run parameters
#####################################

###########Source (load) the  functions
source (paste(wdir_functions,"/Function_GetExtent.R", sep=""))
source (paste(wdir_functions,"/Function_LOCASummmarize.R", sep=""))
source (paste(wdir_functions,"/Function_Gen_YearSum.R", sep=""))
source (paste(wdir_functions,"/Function_General.R", sep=""))
source (paste(wdir_functions,"/Function_Gen_DiffHist.R", sep=""))
###########

#Set variable for the working directory for Clip Boundary GIS file
#wdir_clipbound = BoundaryFolderName

#Setup folder and shapefile list files
FolderNameList = c(wdir_clipbound)
ShapeFileList = c(AFB_Name)


#Get lat/lon min/max using getextent funcion
#Call function to get bounding box of extent of all shapefile
BBox_DF = GetExtent(FolderNameList,ShapeFileList)

#Assign extent points from bounding box
lat_max = BBox_DF["y","max"]
lat_min = BBox_DF["y","min"]
lon_max = BBox_DF["x","max"]
lon_min = BBox_DF["x","min"]

#Adjust search area based on searc_buff value if needed for very small boundaries (1mi = .015deg)
lat_max = lat_max + search_buff *.015
lat_min = lat_min - search_buff *.015
lon_max = lon_max + search_buff *.015
lon_min = lon_min - search_buff *.015



#######-------------  Master Loop for scenarios / years combo (ends at bottom)

for (iarray in 1:5) 
{
  
scenario = scenario_yr_array[1,1] [iarray,1]
start_yr = scenario_yr_array[1,2] [iarray,2]
end_yr = scenario_yr_array[1,3] [iarray,3]
desired_yrs = paste(start_yr,"-",end_yr,sep="")


loopyr_start= start_yr
loopyr_end = end_yr

firstloop = TRUE

#-------------loop for multiple years ... Ends at bottom
for (yr in loopyr_start:loopyr_end) 
  
{
  desired_yr = as.character(yr)

  #Set the working directory for NetCDF files
  setwd(paste(wdir_netcdf, "\\", scenario, sep=""))

  #Assign year grouing for this year for determining Netcdf file to retrieve
    
  if (scenario != "historical" & yr > 2005 & yr < 2011) {
    yr_group_start == 2006
    yr_group_end == 2010
  }
   else {
    yr_remainder = yr %% 10
    yr_group_start = yr - yr_remainder + 1
    yr_group_end = yr_group_start + 9
   }
  

  #Set the model netcdf file name
  suffix_file = paste("_day_HadGEM2-ES_",scenario,"_r1i1p1_EWEMBI_",yr_group_start,"0101-",yr_group_end,"1231.nc4",sep="")
  nctmax_file = paste("tasmax",suffix_file,sep="")
  nctmin_file = paste("tasmin",suffix_file,sep="")
  ncprcp_file = paste("pr",suffix_file,sep="")
  



#extract netcdf to data object
tmax.nc = nc_open(nctmax_file)
tmin.nc = nc_open(nctmin_file)
#tdmean.nc = nc_open(nctdmean_file)
prcp.nc = nc_open(ncprcp_file)

#Check for 1st year of loop and get lat/long start/cnt for 1st year loop only

lon_start <- min(which(lon > lon_min))
  


if (yr == loopyr_start)
{
  
  #Get lon  starts and count indexes based on lon range
  lon = ncvar_get( tmax.nc, "lon")
  for (indx in (1:dim(lon)))  # Loop to get lon start index
  {
    if (lon[indx] > lon_min)
    {
      lon_start  = lon[indx] - 1
      for (indx2 in (lon_start: dim(lon))) #Loop to get lon count index
      {
        if (lon[indx2] > lon_max)
        {
          lon_count = indx2 - lon_start
          break
        } ## End 2nd If
        
      } ## End 2nd For
      break
    } ## End 1st If
  } ## End 1st For
  
  #Get lat  starts and count indexes based on lat range
  lat = ncvar_get( tmax.nc, "lat")
  for (indx in (1:dim(lat)))  # Loop to get lat start index
  {
    if (lat[indx] > lat_min )
    {
      lat_start  = indx - 1
      for (indx2 in (lat_start: dim(lat))) #Loop to get lat count index
      {
        if (lat[indx2] > lat_max)
        {
          lat_count = indx2 - lat_start
          break
        } ## End 2nd If
        
      } ## End 2nd For
      break
    } ## End 1st If
  } ## End 1st For
  
} #End check for start year 


#Use this to calculate time values

#Time is for entire period starting at index=1 with count as number of days in that year
time_start = 1
time_count =  julian(1,1,as.numeric(desired_yr) + 1, origin=c(month=1,day=1,year=as.numeric(desired_yr)))

#Set variable for output CSV file name
fname_outputcsv = paste("Allvalues_", desired_yr, ".csv", sep="")


#load netcdf values into variables (note xvar has 3 dimensions: lon,lat,time)
lon = ncvar_get( tmax.nc, "lon", start=c(lon_start), count=c(lon_count))
lat = ncvar_get( tmax.nc, "lat", start=c(lat_start), count=c(lat_count))
time = ncvar_get( tmax.nc, "time", start=c(time_start), count=c(time_count))
xtmax = ncvar_get( tmax.nc, "tasmax", start=c(lon_start,lat_start,time_start), count=c(lon_count,lat_count,time_count))
xtmin = ncvar_get( tmin.nc, "tasmin", start=c(lon_start,lat_start,time_start), count=c(lon_count,lat_count,time_count))
#xtdmean =  ncvar_get( tdmean.nc, "tdmean", start=c(lon_start,lat_start,time_start), count=c(lon_count,lat_count,time_count))
xprcp = ncvar_get( prcp.nc, "pr", start=c(lon_start,lat_start,time_start), count=c(lon_count,lat_count,time_count))

# rm(tmax.nc)
# rm(tmin.nc)
# rm(prcp.nc)

#convert to dataframe
#first initialize arrays for lon, lat, time, and xvar
lxtmax = length(xtmax)
arraylon = c(rep(1.1,lxtmax))
arraylat = c(rep(1.1,lxtmax))
arraytime = c(rep(1,lxtmax))
arraytmax = c(rep(1.1,lxtmax))
arraytmin = c(rep(1.1,lxtmax))
arrayprcp = c(rep(1.1,lxtmax))
#arraygdd = c(rep(0.0,lxtmax))

#Run loops to load arrays for dataframe
i = 0
for (ilon in 1:lon_count)
 {	
	for (ilat in 1:lat_count) 
	{
		for (itime in 1:time_count)
		{
			i = i + 1
			
			#Asign arrays for lat, lon, time
			arraylon[i] = lon[ilon]	- 360	
			arraylat[i] = lat[ilat]
			arraytime[i] = time[itime]

			# Assign temp and precip 
			arraytmax[i] = round(xtmax[ilon, ilat, itime],5)
			arraytmin[i] = round(xtmin[ilon, ilat, itime],5)
			arrayprcp[i] = round(xprcp[ilon, ilat, itime],9)
			

		}
	}
  } #END loops to load arrays for dataframe

#clear xvar object from memory
rm(xtmax,xtmin,xprcp)



#Create the dataframe
#newdf = data.frame(lat=arraylat, lon=arraylon, time=arraytime, tmax=arraytmax, tmin=arraytmin, tdmean = arraytdmean, prcp=arrayprcp)
#newdf = data.frame(lat=arraylat, lon=arraylon, time=arraytime, tmax=arraytmax, tmin=arraytmin, prcp=arrayprcp, gdd=arraygdd)
newdf = data.frame(lat=arraylat, lon=arraylon, time=arraytime, tmax=arraytmax, tmin=arraytmin, prcp=arrayprcp)

#clear all array variables
rm(arraylat,arraylon,arraytime,arraytmax,arraytmin,arrayprcp)



#Add the date field to data frame
newdf$date <- format(as.Date(chron(newdf$time, origin=c(month=1,day=1,year=1900))), "%m/%d/%Y")

#Convert degK to degC for tmax and tmin
newdf$tmax = round(RasterUnitConvert(newdf$tmax,"KtoC"),5)
newdf$tmin = round(RasterUnitConvert(newdf$tmin,"KtoC"),5)

#Convert Precip from kg/m2/s to mm
newdf$prcp = RasterUnitConvert(newdf$prcp,"KG_M_2_S_1toMM_DAY")


#----------------Check and perform clip type requested
ClipDone = FALSE

# ---------Try clip with polygon, but if not big enough (no clip values), perform with Point-Centroid
while(!ClipDone)
{

# ------- For Polygon Clipping - 
if (clip_type == "Polygon")
{
  #Polygon Clip
  
  #show status
  cat("\n",paste("Clipping data - try polygon..Year= ", yr, "  System time is ", Sys.time()),"\n")
  flush.console()

  #Create new data frame for spatial points
  new_spatial_df = newdf
  
  #  turn dataframe into a SpatialPointsDataFrame
  coordinates(new_spatial_df) <- c("lon", "lat")
  
  #Set the working directory for Clip Boundary GIS file
  setwd(wdir_clipbound)
  
  # read in boundary polygons
  Clip_Bnd <- readOGR(".", AFB_Name)
  
  # tell R that coordinates are in the same lat/lon reference system
  # as the Clip_Bnd data -- BUT ONLY BECAUSE WE KNOW THIS IS THE CASE!
  proj4string(new_spatial_df) <- proj4string(Clip_Bnd)
  
  # combine is.na() with over() to do the containment test; note that we
  # need to "demote" Clip_Bnd to a SpatialPolygons object first
  inside.Clip_Bnd <- !is.na(over(new_spatial_df, as(Clip_Bnd, "SpatialPolygons")))
  
  #Create new data frame with clip values only
  clipDF = new_spatial_df[inside.Clip_Bnd, ]
  
  # If now rows after clip, then use the PointCentroid clipping method
  if (nrow(clipDF) == 0) 
  { 
    ClipDone = FALSE
    clip_type = "PointCentroid"
    
  }else {
    
    ClipDone = TRUE
  }
  
  
## ---------- For Point  or Point Centroid Clipping 
} else if (clip_type == "Point" || clip_type == "PointCentroid")
{  
 
  
  #Select only one point with lat/long nearest specified point
  
  #show status
  cat("\n",paste("Finding nearest lat/lon...Year= ", yr, "  System time is ", Sys.time()),"\n")
  flush.console()
  
  
  #set stn points to centroid if requested
  if (clip_type == "PointCentroid")
  {
    # read in boundary polygons
    setwd(wdir_clipbound)
    Clip_Bnd <- readOGR(".", AFB_Name)
    
    # Get centroids and assign to stn_lon/lat
    cent <- gCentroid(Clip_Bnd, byid=FALSE)
    stn_lon = coordinates(cent)[1]
    stn_lat = coordinates(cent)[2]
    
    
  }  ###  End IF point Centroid
  
  #Only perform search for first year since all remaining years will have same value. 
  if (yr == start_yr)
  {
    
    #First get unique lat/longs
    Unique_DF = data.frame(sqldf(paste('
                                       
      SELECT lat, lon
      FROM newdf
      GROUP BY lat, lon
      ')))
    
    
    dist_closest = 9999999999
    
    #Loop through each record in data frame and find closest lat/lon to station point x/y
    for (irec in 1:nrow(Unique_DF))
    {
      
      # Get distance between hist and proj 
      dist_val = distGeo(c(Unique_DF[irec,"lon"],Unique_DF[irec,"lat"]), c(stn_lon,stn_lat ))
      
      #Find closest distance 
      if (irec == 1 || dist_val <= dist_closest)
      {
        
        #If closest and same month/day then flag as best fit so far   
        dist_closest = dist_val
        index_closest = irec
        close_lon = Unique_DF[irec,"lon"]
        close_lat = Unique_DF[irec,"lat"]
        
      }#End if 
      
    } # End For irec  (Find Closests lat/lon)
    
    close_lat = round( close_lat,6)
    close_lon = round( close_lon,6)
    
    
  } # End if for 1st year only 
  
  
  # Now select only those records with closes lat/ lon
  clipDF = data.frame(sqldf(paste('
                                  SELECT *
                                  FROM newdf
                                  WHERE lon = ',close_lon,' AND lat = ',close_lat
                                  
  )))
  
  ClipDone = TRUE    #Exit the while loop. We've used point clip
  
} # End If for Clip type   
  
} # End While


#Clear full spatial data frame
#write.csv(newdf, "testFULL1.csv", row.names=FALSE) #DEBUG
rm(newdf)
#rm(new_spatial_df)


#Add additional year data to allvalues dataframe

if (firstloop) {
  allvaluesdf = as.data.frame(clipDF)
  firstloop = FALSE 
} else {
  allvaluesdf = rbind(allvaluesdf,as.data.frame(clipDF))
}


cat("\n",paste("Completed ", AFB_Name, scenario, yr, "System time is ", Sys.time(), sep=" "),"\n")
flush.console()

  
} #----------------- End Loop for years

#Create the output directory if it doesn't exist
dir.create(paste(wdir_outputcsv,"/", AFB_Name,sep=""), recursive = TRUE,  mode = "0777")


#Set the working directory for Clip Boundary GIS file
#setwd(paste(wdir_outputcsv,"/", AFB_Name,sep=""))

#Write out csv for scenario-years combo
#fname_outputcsv = paste("Allvalues_", AFB_Name, "_", scenario, "_", start_yr, "-", end_yr, ".csv", sep="")
#write.csv(allvaluesdf, fname_outputcsv, row.names=FALSE)

#Call function to summarize and print this scenario-years data
LOCASummarize(allvaluesdf,AFB_Name,scenario,desired_yrs,wdir_outputcsv,wdir_functions)


} #---------------------End Loop for start year / end year / scenarios combo

#Call function to generate historical difference files for all RCP/Years
Gen_DiffHist(AFB_Name,scenario_yr_array,wdir_outputcsv,wdir_functions,"LOCA")

#Call function to generate year sum file for all years/scenarios
Gen_YearSum(AFB_Name,scenario_yr_array,wdir_outputcsv,wdir_functions)

#clean out large objects
rm(tmax.nc)
rm(tmin.nc)
#rm(tdmean.nc)
rm(prcp.nc)

cat("\n",paste("**** Script complete for ", AFB_Name, " - End time is ", Sys.time()),"\n")