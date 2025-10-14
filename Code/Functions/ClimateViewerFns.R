################################################################################
###           FUNCTIONS FOR VHOTDAYS, EXHOTDAYS, WARMNIGHTS,                ###
###           FRFRDAYS & VWETDAYS                                           ###
################################################################################

# written by Annie Kellner 2-7-2025
# for Climate Viewer inputs


fnHOTDAYS <- function(tmaxrast, lo = 32.2, hi = 35) 
{
  
  #Find all Tmax values greater than or equal to hottemp
  tmaxrast[tmaxrast >= lo & tmaxrast < hi] = 1
  
  #Set all remaining values to 0
  tmaxrast[tmaxrast != 1] = 0
  
  return(tmaxrast)
  
  # Assign hotdays as sum of raster stack
  #hotdays = sum(tmaxrast)  
  
}


fnVHOTDAYS <- function(tmaxrast, lo = 35, hi = 37.78){
    
    #Find all Tmax values greater than or equal to hottemp
    tmaxrast[tmaxrast >= lo & tmaxrast < hi] = 1
    
    #Set all remaining values to 0
    tmaxrast[tmaxrast != 1] = 0
    
    return(tmaxrast)
    
    # Assign hotdays as sum of raster stack
    #hotdays = sum(tmaxrast)  
    
  }


fnEXHOTDAYS <- function(tmaxrast, lo = 37.8, hi = 40.6){
    
    #Find all Tmax values greater than or equal to hottemp
    tmaxrast[tmaxrast >= lo & tmaxrast < hi] = 1
    
    #Set all remaining values to 0
    tmaxrast[tmaxrast != 1] = 0
    
    return(tmaxrast)
    
    # Assign hotdays as sum of raster stack
    #hotdays = sum(tmaxrast)  
    
}


fnHELLDAYS <- function(tmaxrast, lo = 40.6){
  
  #Find all Tmax values greater than or equal to hottemp
  tmaxrast[tmaxrast >= lo] = 1
  
  #Set all remaining values to 0
  tmaxrast[tmaxrast != 1] = 0
  
  return(tmaxrast)
  
  # Assign hotdays as sum of raster stack
  #hotdays = sum(tmaxrast) 
}


fnWARMNIGHTS <- function(tminrast, coldtemp = 23.9){
  
  #Find all Tmin values greater than or equal to coldtemp
  tminrast[tminrast > coldtemp] = 1
  
  #Set all remaining values to 0
  tminrast[tminrast != 1] = 0
  
  return(tminrast)
  
  # Assign colddays as sum of raster stack
  #colddays = sum(tminrast)
}


fnFRFRDAYS <- function(tminrast, coldtemp = 0){
 
  #Find all Tmin values greater than or equal to coldtemp
  tminrast[tminrast > coldtemp] = 1
  
  #Set all remaining values to 0
  tminrast[tminrast != 1] = 0
  
  return(tminrast)
  
  # Assign colddays as sum of raster stack
  #colddays = sum(tminrast) 
}
 

fnVWETDAYS <- function(preciprast, wetprecip = 101.6) 
{
  
# Set unwanted values to NA
  preciprast[preciprast > wetprecip] = 1
  
  # Find all Precip values greater than or equal to wetprecip
  preciprast[preciprast != 1] = 0
  
  # Assign wetddays as sum of raster stack
  #wetdays = sum(preciprast, na.rm=TRUE )
  
  return(preciprast)
  
}


