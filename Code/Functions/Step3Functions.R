############################################
#######   STEP 3 FUNCTIONS  ################
############################################

#' Add variables to MonthSum from MACA analyses
#'  
#'  @param AllDaysDF AllDays dataframe from MACA analysis            

getMACAcols <- function(AllDaysDF){
  MACAcols = AllDaysDF %>%
    dplyr::select(Avg_Rad,
                  Avg_EastWind,
                  Avg_NorthWind,
                  Avg_WindSpeed,
                  Avg_WindDirDeg,
                  Avg_SH,
                  Avg_VPdeficit,
                  Avg_VPsat,
                  Avg_VPamb,
                  Avg_RH)
}

#'  RELATIVE HUMIDITY
#'  Example use:
#'  RH_min = calcRH(h, T_max, Q)
#'  RH_max = calcRH(h, T_min, Q)
#'
#'  @param h altitude to calculate pressure
#'  @param T Temperature at altitude (K)
#'  @param Q specific humidity (SpecH)

calcP <- function(h, T){
  ## Barometric formula
  ## See https://en.wikipedia.org/wiki/Barometric_formula
  ## Compare against https://www.omnicalculator.com/physics/air-pressure-at-altitude
  ##
  ## h = altitude to calculate pressure (m)
  ## T = temperature at the altitude (K)
  
  g <- 9.80665 # Gravitational acceleration, m/s2
  
  M <- 0.0289644 # Molar mass Earth atmo, kg/mol
  
  R <- 8.31432 # Universal gas constant, N*m/mol*K
  
  h_0 <- 0 # Reference level, assume sea level
  
  P_0 <- 101325 # Reference pressure, assume sea level, in Pascals
  
  P <- P_0*exp(-g*M*(h-h_0)/(R*T))
  
  return(P)
}

calcRH <- function(h, T, Q){
  ## Calculates relative humidity
  ## How test?? Need a table with h, T, dimensionless Q, and RH?
  ##
  ## T = temperature (K); max RH at min T, min RH at max T
  ## h = altitude to calculate pressure (m)
  ## Q = specific humidity (dimensionless)
  
  T_0 <- 273.16 # reference temperature in K
  
  P <- calcP(h, T)
  
  RH <- 0.263*P*Q*(exp((17.67*(T - T_0))/(T - 29.65)))^-1
  
  return(RH)
}