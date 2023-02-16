##################################################
###   FUNCTIONS USED FOR STEPS PROCESS  ##########
##################################################

#' RasterUnitConvert
#' 
#' Create new values reflecting unit conversion (add attribute? Or mutate?)
#' Convert argument defines what conversion is done
#' 
#' @param value_from Value to be converted
#' @param ConvertType Metrics to convert

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

#' RasterGDD
#' 
#' Description: Create a raster of total growing degree days given two raster stacks
#'              of daily Tmax and Tmin values. Rasters must be stacked with layers that
#'             correspond to same day (layer1 represents day 1, Layer2 day2, etc in each stack).
#'              Return raster will contain values of total GDD for the time period.
#'              
#' @param tminrast Raster stack of minimum temperature values (Tmin) in Deg Celsius with each layer corresponding to a day.
#' @param tmaxrast Raster stack of maximum temperature values (Tmax) in Deg Celsius with each layer corresponding to a day.
#' @param basetemp The base temperature in Deg Celsius used in GDD calculation
#' @param captemp = The maximum cap temperature in Deg Ceslcius used in GDD calculation

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

              



