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

