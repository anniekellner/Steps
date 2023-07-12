#------------------------------------------------------------------------------------------------------------
# author: Thomas E. Hilinski <tom.hilinski@colostate.edu>
# adapted by: Annie Kellner <annie.kellner@colostate.edu> 7/12/23
# license:      See LICENSE.md or https://unlicense.org

# This script creates Walter-Leith diagrams (1963). It is meant to be used in conjunction with
# Step3.Rmd as part of a simplified workflow

#------------------------------------------------------------------------------------------------------------

library(devtools)
library(grid)
library(gridExtra)

source("MiscUtils.R") 
source("TextProcessing.R") 
source("Units.R") 
source("ClimateVars.R") 
source("PlotBase.R") 
source("WLDiagram.R") 

#--------------------------------------------------------------------
# configuration
setwd("N:/RStor/mindyc/afccm/Climate Modeling/Software Apps/R scripts/Trevor/WLDiagram-master/WLDiagram-master/")
config <- list ( width  = 2000,
                 height = 2000,
                 orientation = "portrait" )	# valid orientation: portrait, landscape

experiment  <- "historical"
yearRange   <- c( 1976, 2005 )
#  or
#experiment  <- "rcp45"
#experiment  <- "rcp85"
#yearRange   <- c( 2026, 2035 )
#yearRange   <- c( 2046, 2055 )

# load per-month data
regionName  <- "Homestead ARB"
pm.df <- read.csv("Homestead_ARB_dataPerMonth.csv")	# units are SI
#write.csv(pm.df,"data_PerMonth.csv")

# names(pm.df)
#   [1] "month"                  "Temp_historical"        "Temp_rcp45_2026_2035"
#   [4] "Temp_rcp45_2046_2055"   "Temp_rcp85_2026_2035"   "Temp_rcp85_2046_2055"
#   [7] "Tmax_historical"        "Tmax_rcp45_2026_2035"   "Tmax_rcp45_2046_2055"
#  [10] "Tmax_rcp85_2026_2035"   "Tmax_rcp85_2046_2055"   "Tmin_historical"
#  [13] "Tmin_rcp45_2026_2035"   "Tmin_rcp45_2046_2055"   "Tmin_rcp85_2026_2035"
#  [16] "Tmin_rcp85_2046_2055"   "ATmn_historical"        "ATmn_rcp45_2026_2035"
#  [19] "ATmn_rcp45_2046_2055"   "ATmn_rcp85_2026_2035"   "ATmn_rcp85_2046_2055"
#  [22] "Precip_historical"      "Precip_rcp45_2026_2035" "Precip_rcp45_2046_2055"
#  [25] "Precip_rcp85_2026_2035" "Precip_rcp85_2046_2055"

# get RCP4.5 2030 decade:
i <- grep( experiment, names(pm.df) )
stopifnot( length(i) > 0 )
if ( experiment != "historical" )
{
  pm.df <- pm.df[,i]
  i <- grep( toString(yearRange[2]), names(pm.df) )
}
stopifnot( length(i) > 0 )
pm.df <- pm.df[,i]
iTmin <- grep( "Tmin", names(pm.df) )
iTmax <- grep( "Tmax", names(pm.df) )
iATmn <- grep( "ATmn", names(pm.df) )
iPrec <- grep( "Precip", names(pm.df) )
# iTmin; iTmax; iATmn; iPrec ==  3  2  4  5

# make diagram
message( "Plot 1: SI units" )
wldiag.si <- WLDiagram( regionName, experiment, yearRange,
                        pm.df[,iPrec], pm.df[,iTmin], pm.df[,iTmax], pm.df[,iATmn],
                        isUnitsSI=TRUE )

message( "Plot 2: Imperial units" )
wldiag.im <- WLDiagram( regionName, experiment, yearRange,
                        pm.df[,iPrec], pm.df[,iTmin], pm.df[,iTmax], pm.df[,iATmn],
                        isUnitsSI=FALSE )

message( "\nAbout this plot:" )
message( "Description:" )
writeLines( Para( wldiag.si$Describe(), sep="\n  ", prefix="  " ) )
message( "Caption:" )
writeLines( Para( wldiag.si$Caption(), sep="\n  ", prefix="  " ) )

message( "Data (mm and C):")
print( round( wldiag.si$df, digits=1 ) )

message( "Data (in and F):")
print( round( wldiag.im$df, digits=1 ) )

message( "\nMaking plots..." )

# class WLDiagram uses the base graphics package to draw the graph
dev.new( width=10.0, height=5.0 )
grobs.list <- list(wldiag.im$Plot( wldiag.im$MakeConfig( config$plots$AsList() ) ) )
grid.arrange( grobs = grobs.list, ncol=1 )

PauseForKeypress( nlBefore=TRUE )
dev.off()

# all done!
message( "\n   all done!\n" )

# end
