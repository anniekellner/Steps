######################################################################
# Function: LOCASumarize
# Author: Bob Flynn 7/1/2020
# Description: Summarizes LOCA daily data for a given site writes csv files for these summaries:
#  AllDays.csv - all input data plus some derived values
#  AllDays_SiteAvg.csv - All daily data summarized by each day to give averages for the entire site.
#  YearMonthSum.csv - all data summarized by year and month groupings.
#  MonthSum.csv - all data summarized by month groupings.
#
# Parmameters:
#   allvaluesdf = dataframe containin daily raw data for the site.
#   AFB_Name - the name of the Air Force Base
#   Scenario - the data scenario ("historical", "rcp45", "rcp85")
#   desired years - year range for analysis ("1976-2005", "2026-2035", "2046-2055")
#   wdir_outputcsv - directory where csv files will be written
#   wdir_functions - directory where R functions script files reside
#
LOCASummarize <- function(allvaluesdf,AFB_Name,scenario,desired_yrs,wdir_outputcsv,wdir_functions) 
{  
require(chron)
require (sqldf)


#Load functions
# source("C:\\Users\\bobfl\\OneDrive\\Documents\\CSU\\CEMML 2020\\Test Env\\Bob-R-Src\\Functions\\main_functions.R")
source(paste(wdir_functions,"/Function_General.R",sep=""))


# Set working directory to the output CSV file location
#setwd(paste(wdir_outputcsv, "/", AFB_Name,sep=""))
      

# Get start and end years and total number of years 
start_yr = as.numeric(substr(desired_yrs,1,4))
end_yr = as.numeric(substr(desired_yrs, 6,9))
total_yrs = end_yr - start_yr + 1

#Set the working directory for original files files
#setwd(wdir_inputcsv)

#Calculate Celsius Temperatures and add column  
allvaluesdf$TMaxC = allvaluesdf$tmax
allvaluesdf$TMinC = allvaluesdf$tmin
allvaluesdf$TMeanC = (allvaluesdf$TMaxC + allvaluesdf$TMinC) /2

#Calculate FAhrenheit Temperatures and add column
allvaluesdf$TMaxF = RasterUnitConvert(allvaluesdf$tmax,"CtoF")
allvaluesdf$TMinF = RasterUnitConvert(allvaluesdf$tmin,"CtoF")
allvaluesdf$TMeanF= (allvaluesdf$TMaxF + allvaluesdf$TMinF) /2

# allvaluesdf$TMaxF = allvaluesdf$tmax *1.8 + 32
# allvaluesdf$TMinF = allvaluesdf$tmin *1.8 + 32


#Calculate Precip in inches and add column
allvaluesdf$PPT_mm = allvaluesdf$prcp
allvaluesdf$PPT_in = RasterUnitConvert(allvaluesdf$prcp,"MMtoIN")
#allvaluesdf$PPT_in = allvaluesdf$prcp / 25.4

#Add the year/month fields using the date field
allvaluesdf$year <- format(as.Date(allvaluesdf$date, "%m/%d/%Y"),"%Y")
allvaluesdf$month <- format(as.Date(allvaluesdf$date, "%m/%d/%Y"),"%m")
allvaluesdf$day <- format(as.Date(allvaluesdf$date, "%m/%d/%Y"),"%d")

#Calculate and add the YearMonth Column
allvaluesdf$YearMonth = paste(allvaluesdf$year, formatC(allvaluesdf$month, width = 2, flag = '0'), sep="-")

#Calculate/add Count values (GDD, hotday,colddays,wetdays, drydays,ftdays) using function call
allvaluesdf$GDDF = RasterGDD(allvaluesdf$TMinF,allvaluesdf$TMaxF,50,86)
allvaluesdf$hotdays = Rasterhotdays(allvaluesdf$TMaxC)
allvaluesdf$colddays = Rastercolddays(allvaluesdf$TMinC)
allvaluesdf$wetdays = Rasterwetdays(allvaluesdf$pr)
allvaluesdf$drydays = Rasterdrydays(allvaluesdf$pr)
allvaluesdf$ftdays = RasterFTdays(allvaluesdf$TMaxC, allvaluesdf$TMinC)

#Create All Days dataframe for writing to csv file
AllDaysdf = data.frame(sqldf(paste("select lat, lon, date, year, month, YearMonth,
                                  PPT_in,
                                  PPT_mm,
                                  TMaxF , 
                                  TMinF, 
                                  TMeanF, 
                                  GDDF,
                                  hotdays,
                                  colddays,
                                  wetdays,
                                  drydays,
                                  ftdays 
                                  
                                  
                                  from allvaluesdf 
                                  
                                 ", sep="")))



#Write out ALLVALUESDF to CSV file, but remove the year, month, and yearmonth columns
write.csv(AllDaysdf[,-c(4:6)], paste(AFB_Name, "_", scenario,"_", desired_yrs,"_AllDays.csv",sep=""), row.names=FALSE)


#Create all data by date so all location points are averaged for the site (for csv writing only)
 
AllDays_AVGdf = data.frame(sqldf(paste("select date, year, month, YearMonth, 
                                  avg(PPT_mm) as PPT_mm,
                                  AVg(PPT_in) as PPT_in,
                                  AVg(TMaxF) as TMaxF,
                                  AVg(TMinF) as TMinF,
                                  AVg(TMeanF) as TMeanF,
                                  AVg(GDDF) as GDDF,
                                  AVg(hotdays) as hotdays,
                                  AVg(colddays) as colddays,
                                  AVg(wetdays) as wetdays,
                                  AVg(drydays) as drydays, 
                                  AVg(ftdays) as ftdays
                                  
                                  from AllDaysdf 
                                  group by date
                                  
                                 ", sep="")))


#Write out ALldays_AVgdf (the site average) to CSV file (but remove year, month, and yearmonth)
write.csv(AllDays_AVGdf[-c(2:4)], paste(AFB_Name, "_", scenario,"_", desired_yrs,"_AllDays_SiteAvg.csv",sep=""), row.names=FALSE)



#Create summarized Dataframe for values grouped by YearMonth
YearMonthSumDF = data.frame(sqldf(paste("select year, month,  
                                  sum([PPT_in]) as Total_PPT_in,
                                  sum([PPT_mm]) as Total_PPT_mm,
                                  avg([TMaxF]) as Avg_TmaxF, 
                                  avg([TMinF]) as Avg_TminF, 
                                  avg(TMeanF) as Avg_TMeanF, 
                                  Min([TMinF]) as Abs_TminF, 
                                  sum([GDDF]) as Total_GDDF,
                                  sum([hotdays]) as Total_hotdays,
                                  sum([colddays]) as Total_colddays,
                                  sum([wetdays]) as Total_wetdays,
                                  sum([drydays]) as Total_drydays,
                                  sum([ftdays]) as Total_ftdays 
                                   
                                  
                                  from AllDays_AVGdf 
                                  
                                  group by YearMonth ", sep="")))



#Write out the YearMonthSum to CSV file
write.csv(YearMonthSumDF, paste(AFB_Name, "_", scenario,"_", desired_yrs, "_YearMonthSum.csv",sep=""), row.names=FALSE)
          

#GENERATE MONTHSUM data frame
MonthSumDF = data.frame(sqldf(paste("select month, 
                                  sum([PPT_in]) as Avg_PPT_in,
                                  sum([PPT_mm]) as Avg_PPT_mm,
                                  avg([TMaxF]) as Avg_TmaxF, 
                                  avg([TMinF]) as Avg_TminF, 
                                  avg(TMeanF) as Avg_TMeanF, 
                                  Min([TMinF]) as Abs_TminF, 
                                  sum([GDDF]) as Avg_GDDF,
                                  sum([hotdays]) as Avg_hotdays,
                                  sum([colddays]) as Avg_colddays,
                                  sum([wetdays]) as Avg_wetdays,
                                  sum([drydays]) as Avg_drydays, 
                                  sum([ftdays]) as Avg_ftdays 
                                  
                                  from AllDays_AVGdf

                                  group by month ", sep="")))

#Recalculate averages for count values based on number of years
MonthSumDF$Avg_PPT_in = MonthSumDF$Avg_PPT_in / total_yrs
MonthSumDF$Avg_PPT_mm = MonthSumDF$Avg_PPT_mm / total_yrs
MonthSumDF$Avg_GDDF = MonthSumDF$Avg_GDDF / total_yrs
MonthSumDF$Avg_hotdays = MonthSumDF$Avg_hotdays / total_yrs
MonthSumDF$Avg_colddays = MonthSumDF$Avg_colddays / total_yrs
MonthSumDF$Avg_wetdays = MonthSumDF$Avg_wetdays / total_yrs
MonthSumDF$Avg_drydays = MonthSumDF$Avg_drydays / total_yrs
MonthSumDF$Avg_ftdays = MonthSumDF$Avg_ftdays / total_yrs


#Calculate/Add row of year summary for averages and totals
MonthSumDF = rbind(MonthSumDF, data.frame(month="Yr Average",t(colMeans(MonthSumDF[,-1]))))
MonthSumDF = rbind(MonthSumDF, data.frame(month="Yr Totals",t(colSums(MonthSumDF[-13,-1]))))

#For misleading averages and totals, replace with NA (ie, temperature totals, ppt averages, etc)
MonthSumDF[13,c(2:3,7:13)] = "NA"
MonthSumDF[14,4:7] = "NA"


#Write the month sum CSV file
write.csv(MonthSumDF, paste(AFB_Name, "_", scenario,"_", desired_yrs, "_MonthSum.csv",sep=""), row.names=FALSE)


#cat("\n",paste("**** Function complete - End time is ", Sys.time()),"\n")
cat("\n",paste("Summary Files Written", AFB_Name, scenario, desired_yrs,"Start time is", Sys.time(),sep=" "),"\n")

}
