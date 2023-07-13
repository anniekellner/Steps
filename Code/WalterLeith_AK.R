###################################################
###     WALTER & LEITH DIAGRAMS   #################
###################################################

# inputs:
  # monthSum dataframe
  # years
  # scenarios

source("./Code/CEMML/WalterLeith/PlotBase.R") 
source("./Code/CEMML/WalterLeith/WLDiagram.R")
source("./Code/CEMML/WalterLeith/MiscUtils.R") 

# see whether these are necessary
source("./Code/CEMML/WalterLeith/TextProcessing.R") 
source("./Code/CEMML/WalterLeith/Units.R") 
source("./Code/CEMML/WalterLeith/ClimateVars.R") 


## General info for plots

regionName  <- AFB_Name

config <- list (width  = 2000,
                 height = 2000,
                 orientation = "portrait" )	# valid orientation: portrait, landscape

experiment  <- scenarios[1] # ultimately will change to i
yearRange   <- c(years[1], years[2])

# Alter monthSum df to fit requirements for WLDiagram

for(i in 1:length(monthSum)){
  monthSum[[i]] = monthSum[[i]] %>%
    slice_head(n = 12) %>%
    select(Avg_PPT_mm, Avg_TMaxF, Avg_TMinF, Abs_TminF) %>%
    mutate(Avg_TMaxC = RasterUnitConvert(Avg_TMaxF, "FtoC")) %>%
    mutate(Avg_TMinC = RasterUnitConvert(Avg_TMinF, "FtoC")) %>%
    mutate(Abs_TminC = RasterUnitConvert(Abs_TminF, "FtoC")) %>%
    select(Avg_PPT_mm, Avg_TMaxC, Avg_TMinC,Abs_TminC)
}

# Loop through monthSum to create plots

' @field df		data.frame containing 12 columns (one per month) and 4 rows: \cr
#'			prec = mean monthly precip (mm), \cr
#'			tmin = mean maximum monthly temperature (deg C), \cr
#'			tmax = mean minimum monthly temperature (deg C), and \cr
#'			atmn = mean absolute minimum monthly temperature (deg C).
#' @field rcp		string: Historical or RCP name (e.g., "RCP 8.5")
#' @field yearRange	range of years in the dataset which this data represents.
#' @field isUnitsSI	if TRUE show values as SI units, else show English units; default is TRUE.


wldiag.si <- WLDiagram(regionName, experiment, yearRange,
                        monthSum[[i]][,1], monthSum[[i]][,2], monthSum[[i]][,3], monthSum[[i]][,4],
                        isUnitsSI=TRUE)







## Plot 1: SI Units