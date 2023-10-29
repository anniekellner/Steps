
# This is a script to define 3 raster functions (RasterGDD, Rasterhotdays,Rastercolddays)
# and test them with sample data


######################################################################
#Function: RasterGDD
# Description: Create a raster of total growing degree days given two raster stacks
#              of daily Tmax and Tmin values. Rasters must be stacked with layers that
#              correspond to same day (layer1 represents day 1, Layer2 day2, etc in each stack).
#              Return raster will contain values of total GDD for the time period. 
# Parmameters:
#   tminrast = Raster stack of minimum temperature values (Tmin) in Deg Celsius with
#              each layer corresponding to a day.
#   tmaxrast = Raster stack of maximum temperature values (Tmax) in Deg Celsius with
#              each layer corresponding to a day.
#   basetemp = The base temperature in Deg Celsius used in GDD calculation
#   captemp = The maximum cap temperature in Deg Ceslcius used in GDD calculation
#
RasterGDD <- function(tminrast, tmaxrast, basetemp=10, captemp=30) 
{
  
  #If Tmin or tmax values are above captemp or below basetemp, set to those values
  tminrast[tminrast>captemp] = captemp
  tminrast[tminrast<basetemp] = basetemp
  
  tmaxrast[tmaxrast>captemp] = captemp
  tmaxrast[tmaxrast<basetemp] = basetemp

  #Calculate growing degree days based on tmax, tmin and basetemp and sum raster stack
  GDD =  (tmaxrast + tminrast) / 2 - basetemp
  
}
#########################################################################

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
#########################################################################


######################################################################
#Function: Rastercolddays
# Description: Create a raster of values indicating total cold days which by default are
#              days below 32degF (0degC). Input is a raster stack of Tmin values.
#              Rasters must be stacked with layers that correspond to days 
#              (layer1 represents day 1, Layer2 day2, etc in each stack).
#              Returned raster will contain values of total cold days for the time period.
# Parmameters:
#   tminrast = Raster stack of maximum temperature values (Tmin) in Deg Celsius with each
#              layer corresponding to a day.
#   coldtemp = The temperature value in Deg Celsius that defines cold days (cells below
#              this will be flagged as cold days)
#
Rastercolddays <- function(tminrast, coldtemp = 0) 
{
  
  #Find all Tmin values less than or equal to coldtemp
  tminrast[tminrast <= coldtemp] = 1
  
  #Set all remaining values to 0
  tminrast[tminrast != 1] = 0
  
  return(tminrast)
	
  # Assign colddays as sum of raster stack
  #colddays = sum(tminrast)  
  
}
#########################################################################


######################################################################
#Function: Rasterwetdays
# Description: Create a raster of values indicating total wet days which by default are
#              precipitation greater than 1 inch or 25.4 mm. Input is a raster stack of Precip values.
#              Rasters must be stacked with layers that correspond to days 
#              (layer1 represents day 1, Layer2 day2, etc in each stack).
#              Returned raster will contain values of total wet days for the time period.
# Parmameters:
#   preciprast = Raster stack of precipitation temperature values (Precip) in millimeters with each
#              layer corresponding to a day.
#   wetprecip = The Precipitaion value in millimeters that defines wet days (cells above
#              this will be flagged as wet days). Default is 50.8 mm (2 Inches).
#
Rasterwetdays <- function(preciprast, wetprecip = 50.8) 
{
  
  
  # Set unwanted values to NA
  preciprast[preciprast > wetprecip] = 1
  
  # Find all Precip values greater than or equal to wetprecip
  preciprast[preciprast != 1] = 0
  
  # Assign wetddays as sum of raster stack
  #wetdays = sum(preciprast, na.rm=TRUE )
  
  return(preciprast)
  
}
#########################################################################


######################################################################
#Function: Rasterdrydays
# Description: Create a raster of values indicating total dry days which by default are
#              precipitation less than 0.1 inch or 2.54 mm. Input is a raster stack of Precip values.
#              Rasters must be stacked with layers that correspond to days 
#              (layer1 represents day 1, Layer2 day2, etc in each stack).
#              Returned raster will contain values of total wet days for the time period.
# Parmameters:
#   preciprast = Raster stack of precipitation temperature values (Precip) in millimeters with each
#              layer corresponding to a day.
#   dryprecip = The Precipitaion value in millimeters that defines dry days (cells above
#              this will be flagged as wet days). Default is 2.54 mm (0.1 Inch).
#
Rasterdrydays <- function(preciprast, dryprecip = 2.54) 
{
  
  
  # Set unwanted values to NA
  preciprast[preciprast < dryprecip] = 1
  
  # Find all Precip values greater than or equal to wetprecip
  preciprast[preciprast != 1] = 0
  
  # Assign wetddays as sum of raster stack
  #wetdays = sum(preciprast, na.rm=TRUE )
  
  return(preciprast)
  
}
#########################################################################


######################################################################
#Function: Rasterfreezethawdays
# Description: Create a raster of values indicating total freeze thaw days.
#              A freeze-thaw day has Tmin <= freeze threshold and Tmax >= thaw threshold. 
#              Rasters must be stacked with layers that correspond to days 
#              (layer1 represents day 1, Layer2 day2, etc in each stack).
#              Returned raster will contain values of total wet days for the time period.
# Parmameters:
#   tmaxrast = Raster stack of Maximum temperature values in degrees Celsius with each
#              layer corresponding to a day.
#   tminrast = Raster stack of Minimum temperature values in degrees Celsius with each
#              layer corresponding to a day.
#   dryprecip = The Precipitaion value in millimeters that defines wet days (cells above
#              this will be flagged as wet days). Default is 25.4 mm (1 Inch).
#   freezethresh = Freeze threshold value in DegC.
#   thawthresh = Thaw threshold value in DegC
#
RasterFTdays <- function(tmaxrast, tminrast, freezethresh = -2.2, thawthresh = 1.2) 
{
  
  
  # Set unwanted values to NA
  tmaxrast[tmaxrast >= thawthresh & tminrast <= freezethresh] = 1
  #tmaxrast[tmaxrast >= thawthresh] = 1
  
  # Find all Precip values greater than or equal to wetprecip
  tmaxrast[tmaxrast != 1] = 0
  
  
  return(tmaxrast)
  
}
#########################################################################



######################################################################
#Function: RasterUnitConvert
# Description: Create a raster of values indicating New unit conversion.
#              The convert type argument defines what conversion is done. 

#              Returned raster will contain converted values.
# Parmameters:
#   Value_From = Value to be converted
#
#   Value_To = Result Value converted.
#
#   ConvertType = Char field defining conversion type.  The commonly used conversion are:
#                   CtoF - Celsuis to Fahrenheit
#                   CtoK - Celsuis to Kelvin
#                   FtoC - Fahr to Celsius
#                   FtoK - Fahr to Kelvin
#                   KtoC - Kelvin to Celsius
#                   KtoF - Kelvin to Fahr
#                   INtoMM - Inches to MM
#                   MMtoIN - MM to Inches
#                   KG_M_2_S_1toMM_DAY = Kg/m2/s to mm per day (used in LOCA precip data)
#
# Returns the converted values
#
RasterUnitConvert <- function(Value_From, ConvertType="CtoF") 
{
  
  x = Value_From
  
  result <- switch(
    ConvertType,
    KM2toMILES2  = x * 0.38610,
    MILES2toKM2  = x * 2.590003,
    FEETtoMETERS = x * 0.3048,
    METERStoFEET = x * 3.2808,
    DCtoDF       = x * 1.8,
    DFtoDC       = x / 1.8,
    KG_M_2_S_1toMM_DAY = x * 86400.0,
    MM_DAYtoKG_M_2_S_1 = x / 86400.0,
    FtoC         = (x - 32.0) / 1.8,
    FtoK         = (x + 459.67) / 1.8,
    CtoF         = x * 1.8 + 32,
    CtoK         = x + 273.15,
    KtoC         = x - 273.15,
    KtoF         = x * 1.8 - 459.67,
    INtoMM       = x * 25.4,
    MMtoIN       = x * 0.0393701,
    INtoCM       = x * 2.54,
    CMtoIN       = x * 0.3937008,
    MMtoCM       = x * 0.1,
    CMtoMM       = x * 10.0,
    {
      stop( "Error in function RasterUnitConvert. Unknown ConvertType: ", ConvertType )

    })
  
  
  return((result))
  
  
  
}
#########################################################################