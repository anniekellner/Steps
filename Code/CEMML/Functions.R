##################################################
###   FUNCTIONS USED FOR STEPS PROCESS  ##########
##################################################


#' UnitConvert
#' 
#' Create new values reflecting unit conversion (add attribute? Or mutate?)
#' Convert argument defines what conversion is done
#' SPECIFY WHAT IS RETURNED
#' 
#' @param value_from Value to be converted
#' @param value_to Result value converted

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
