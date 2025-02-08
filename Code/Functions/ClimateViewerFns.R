################################################################################
###           FUNCTIONS FOR VHOTDAYS, EXHOTDAYS, WARMNIGHTS,                ###
###           FRFRDAYS & VWETDAYS                                           ###
################################################################################

# written by Annie Kellner 2-7-2025
# for Climate Viewer inputs

VHOTDAYS <- function(tmaxrast, hottemp = 35){
    
    #Find all Tmax values greater than or equal to hottemp
    tmaxrast[tmaxrast >= hottemp] = 1
    
    #Set all remaining values to 0
    tmaxrast[tmaxrast != 1] = 0
    
    return(tmaxrast)
    
    # Assign hotdays as sum of raster stack
    #hotdays = sum(tmaxrast)  
    
  }
}

EXHOTDAYS <- 



















######################################################################
#Function: Rasterhotdays
# Description: Create a raster of  of values indicating total count of hot days which by 
#              default are days above 90degF (32.2degC). Input is a raster stack of Tmax values.
#              Rasters must be stacked with layers that correspond to days
#              (layer1 represents day 1, Layer2 day2, etc in each stack).
#              Returned raster will contain values of total hot days for the time period.
# Parmameters:
#   tmaxrast = Raster stack of maximum temperature values (Tmax) in Deg Celsius with each
#              layer corresponding to a day. 
#   hottemp = The temperature value in Deg Celsius that defines hot days (cells above
#              this will be flagged as hot)
#
Rasterhotdays <- function(tmaxrast, hottemp = 32.2) 
{
  
  #Find all Tmax values greater than or equal to hottemp
  tmaxrast[tmaxrast >= hottemp] = 1
  
  #Set all remaining values to 0
  tmaxrast[tmaxrast != 1] = 0
  
  return(tmaxrast)
  
  # Assign hotdays as sum of raster stack
  #hotdays = sum(tmaxrast)  
  
}