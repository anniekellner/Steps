#------------------------------------------------------------------------------------------------------------
# file:         WLDiagram.R
# project:      ClimatePrimers
# organization: Natural Resource Ecology Laboratory at Colorado State University.
#               Previously: North Central Climate Adaptation Science Center (nccsc.colostate.edu)
# description:  Plot class to draw Walter and Lieth (1960) diagram.
# author:       Thomas E. Hilinski <tom.hilinski@colostate.edu>
# license:      See LICENSE.md or https://unlicense.org
#------------------------------------------------------------------------------------------------------------
# Functions:
#  WLDiagram      Main function to create a plot object.
#  $Name           Returns name of plot; used in configuration file and report.
#  $FileName       Returns a file name to hold the plot. Uses MakeFileName().
#  $MakeConfig     Returns a list of graphical configuration element. Uses MakePlotConfig().
#  $Caption        Returns a caption for the plot.
#  $Describe       Returns a description of the plot for the plot catalog.
#  $Plot           Makes a plot.
#------------------------------------------------------------------------------------------------------------

#' Walter and Lieth diagram
#'
#' Plot class to draw a Walter and Lieth (1960) diagram.
#'
#' @details{
#'   Uses a function based upon climatol::diagwl to draw the diagram.
#'   Package 'grid' draws the plot.
#'   A grob (grid plot object) is returned,
#'   which can then be displayed with 'grid.draw'.
#'
#'   The documentation for climatol::diagwl explains the following.
#'
#'   Climatic data is a 4x12 matrix of monthly (January to December)
#'   data, in the following order: \cr
#'   Row 1: Mean monthly precipitation \cr
#'   Row 2: Mean maximum daily temperature for each month \cr
#'   Row 3: Mean minimum daily temperature for each month \cr
#'   Row 4: Absolute monthly minimum temperature for each month
#'
#'   This last row is only used to determine the probable frost months
#'   (when absolute monthly minimums are equal or lower than 0 C).
#'
#'   As described by Walter and Lieth, when monthly precipitation is
#'   greater than 100 mm, the scale is increased from 2 mm/C to 20 mm/C
#'   to avoid too high diagrams in very wet locations. This change is
#'   indicated by a black horizontal line, and the graph over it is
#'   filled in solid blue.
#'
#'   When the precipitation graph lies under the temperature graph (P <
#'   2T) we have an arid period (filled in dotted red vertical lines).
#'   Otherwise the period is considered wet (filled in blue lines),
#'   unless 'p3line=TRUE', that draws a precipitation black line with a
#'   scale P = 3T; in this case the period in which 3T > P > 2T is
#'   considered semi-arid.
#'
#'   Daily maximum average temperature of the hottest month and daily
#'   minimum average temperature of the coldest month are frequently
#'   used in vegetation studies, and are labeled in black at the left
#'   margin of the diagram.
#' }
#' @references{ Walter, H., Lieth, H., 1960. Klimadiagramma-Weltatlas. G. Fischer Verlag, Jena. }
#' @field df		data.frame containing 12 columns (one per month) and 4 rows: \cr
#'			prec = mean monthly precip (mm), \cr
#'			tmin = mean maximum monthly temperature (deg C), \cr
#'			tmax = mean minimum monthly temperature (deg C), and \cr
#'			atmn = mean absolute minimum monthly temperature (deg C).
#' @field rcp		string: Historical or RCP name (e.g., "RCP 8.5")
#' @field yearRange	range of years in the dataset which this data represents.
#' @field isUnitsSI	if TRUE show values as SI units, else show English units; default is TRUE.
# Can create but not extend:
#' @export WLDiagram
#' @exportClass WLDiagram
#' @concept	plots climate-plots
#' @family plot and map classes
WLDiagram <- setRefClass( "WLDiagram",
    contains = "PlotBase",
    fields = c(	df        = "data.frame",
		rcp       = "character",
		yearRange = "vector",
		isUnitsSI = "logical" ),

    methods = list(

	#' @export
	initialize = function( regionName = NULL, rcp, yearRange,
				prec, tmin, tmax, atmn, isUnitsSI=TRUE, ... )
	{
	    "\\cr
	    WLDiagram constructor.
	     Inherits class \\link{PlotBase}.

	    \\strong{Arguments}
	    \\describe{
	      \\item{ regionName }{
		    \\cr string: name of region, state, or park, or site }
	      \\item{ rcp }{
		    \\cr string: Historical or RCP name (e.g., 'RCP 8.5') }
	      \\item{ yearRange }{
		    \\cr range of years in the dataset which this data represents. }
	      \\item{ prec }{
		    \\cr numeric vector: mean monthly precip (mm); 12 values, one per month. }
	      \\item{ tmin }{
		    \\cr numeric vector: mean minimum monthly temperature (deg C); 12 values, one per month. }
	      \\item{ tmax }{
		    \\cr numeric vector: mean maximum monthly temperature (deg C); 12 values, one per month. }
	      \\item{ atmn }{
		    \\cr numeric vector: mean absolute minimum monthly temperature (deg C); 12 values, one per month. }
	      \\item{ isUnitsSI }{
		    \\cr if TRUE show values as SI units, else if FALSE show English units; default is TRUE }
	      }
	    "

	    # initialize check args for validity and consistency
	    callSuper( regionName=regionName, ... )	# PlotBase::initialize
	    if ( !missing(regionName) && !is.null(regionName) )
	    {
		if ( !missing(rcp) && !IsEmpty(rcp) )
		    .self$rcp <- rcp
		if ( !missing(yearRange) )
		    .self$yearRange <- yearRange
		.self$isUnitsSI <- isUnitsSI
		stopifnot( length(prec) == 12 )
		stopifnot( length(tmin) == 12 )
		stopifnot( length(tmax) == 12 )
		stopifnot( length(atmn) == 12 )
		.self$Prepare( prec, tmin, tmax, atmn )
	    }
	},

	# Make the data.frame in the form required by the diagram function.
	# private
	Prepare = function( prec, tmin, tmax, atmn )
	{
	    #Create a 4x12 dataframe - variable order is significant
	    .self$df <- data.frame( prec=prec, tmax=tmax, tmin=tmin, atmn=atmn )
	    .self$df <- as.data.frame( t( .self$df ) )	# Transpose
	    colnames( .self$df ) <- month.abb		# "Jan", "Feb", ...
	},

	#' @export
	Describe = function( ... )
	{
	    "\\cr
	    Produce a description of the WLDiagram suitable for a plot catalog.

	    \\strong{Arguments}
	    \\describe{
	      \\item{ return }{ \\cr string: plot description }
	      }
	    "

	    if ( IsEmpty(description) )
	    {
		description <<-
		    paste(
			"Draws a Walter and Lieth (1960) diagram using",
			"four monthly time series of 12 values each, one per month:",
			"mean monthly precipitation,",
			"mean of maximum monthly temperatures,",
			"mean of minimum monthly temperatures,",
			"mean absolute minimum monthly temperature." )
	    }
	    return( description )
	},

	#' @export
	Caption = function()
	{
	    "\\cr
	    Produce a caption for the WLDiagram suitable for the climate primer.

	    \\strong{Arguments}
	    \\describe{
	      \\item{ return }{ \\cr string: plot caption }
	      }
	    "

	    if ( IsEmpty(caption) )
	    {
		IsHistorical
		caption <<- paste( "Walter and Lieth (1960) diagram for", regionName )
		if ( IsHistorical(rcp) )
		{
		    caption <<- Sentence( caption,
					  "over the", rcp,
					  "years", paste( yearRange, collapse=" to " ) )
		}
		else
		{
		    caption <<- Sentence( caption,
					  "for the years", paste( yearRange, collapse=" to " ),
					  "and", RCP.DisplayName(rcp) )
		}
	    }
	    return( caption )
	},

	#' @export
	FileName = function( extension="png", path="" )
	{
	    "\\cr
	    Make a plot file name for WLDiagram.

	    \\strong{Arguments}
	    \\describe{
	      \\item{ extension }{ \\cr string: file name extension for type of file; default is png. }
	      \\item{ path }{ \\cr string: plot file path, or NULL if no path }
	      \\item{ return }{ \\cr plot file name }
	      }
	    "

	    if ( IsEmpty(filename) )
	    {
		# by default, use the file name generated by the base class
		filename <<- callSuper( extension, path )
	    }
	    return( filename )
	 },

	#' @export
	MakeConfig = function( ini.plots, ... )
	{
	    "\\cr
	    Make a list of graphical configuration elements.

	    \\strong{Arguments}
	    \\describe{
	      \\item{ ini.plots }{ \\cr list: section [plots] from Configuration.Read }
	      \\item{ ini.colors }{ \\cr list: section [colors] from the configuration file }
	      \\item{ ... }{ \\cr list: optional; additional named plot configuration values }
	      \\item{ return }{ \\cr list of plot configuration values }
	      }
	    "

	    config <- callSuper( ini.plots, ... )
	    # names( config )
	    # [1] "fileName" "title"  "xlab"  "ylab"  "cex"  "width"  "height"  "doLegend"

	    # add any custom elements here
	    # TODO: line colors args: pcol, tcol, pfcol, sfcol
	    # pcol = "#005ac8", tcol = "#e81800"
	    # pfcol = "#79e6e8", sfcol = "#09a0d1"

	    args <- list(...)
	    if ( length(args) > 0 )
		config <- append( config, args )

	    return( config )
	},

	# @title{ Walter & Lieth climatic diagram }
	# @description{ Plot of a Walter && Lieth climatic diagram of a station. }
	# @details{
	#   Climatic data must be passed as a 4x12 matrix of monthly (January to December)
	#   data, in the following order: \cr
	#   Row 1: Mean precipitation \cr
	#   Row 2: Mean maximum daily temperature \cr
	#   Row 3: Mean minimum daily temperature \cr
	#   Row 4: Absolute monthly minimum temperature
	#
	#   This last row is only used to determine the probable frost months
	#   (when absolute monthly minimums are equal or lower than 0 C).
	#
	#   For stations located in the southern hemisphere it is useful to
	#   set 'shem=TRUE', in order to keep the summer period in the central
	#   zone of the graphic (the diagram will begin the plot with the July
	#   data).
	#
	#   As described by Walter and Lieth, when monthly precipitation is
	#   greater than 100 mm, the scale is increased from 2 mm/C to 20 mm/C
	#   to avoid too high diagrams in very wet locations. This change is
	#   indicated by a black horizontal line, and the graph over it is
	#   filled in solid blue.
	#
	#   When the precipitation graph lies under the temperature graph (P <
	#   2T) we have an arid period (filled in dotted red vertical lines).
	#   Otherwise the period is considered wet (filled in blue lines),
	#   unless 'p3line=TRUE', that draws a precipitation black line with a
	#   scale P = 3T; in this case the period in which 3T > P > 2T is
	#   considered semi-arid.
	#
	#   Daily maximum average temperature of the hottest month and daily
	#   minimum average temperature of the coldest month are frequently
	#   used in vegetation studies, and are labeled in black at the left
	#   margin of the diagram.
	# }
	# @param dat		Monthly climatic data for which the diagram will be plotted.
	# @param est		Name of the climatological station
	# @param alt		Altitude of the climatological station
	# @param per		Period on which the averages have been computed
	# @param mlab		Language for month labels:
	#			"en" = Month initials in English (default);
	#			"es" = Month initials in Spanish;
	#			other or NULL = Numeric labels (1-12).
	# @param unitsSI	logical: if TRUE show values as SI units, else if FALSE show English units;
	#			default is TRUE.
	# @param pcol		Color pen for precipitation; default is #005ac8
	# @param tcol		Color pen for temperature; default is #e81800
	# @param pfcol		Fill color for probable frosts; default is #79e6e8
	# @param sfcol		Fill color for sure frosts; default is #09a0d1
	# @param shem		Set to TRUE for southern hemisphere stations; default is FALSE.
	# @param p3line		Set to TRUE to draw a supplementary precipitation line
	#			referenced to three times the temperature (as suggested by
	#			Bogdan Rosca); default is FALSE.
	# @references{ WALTER H & LIETH H (1960): Klimadiagramm Weltatlas. G. Fischer, Jena. }
	#
	# LICENSE
	#   This function was modified the from the climatol::diagwl;
	#   see the R package climatol for the original.
	#   The climatol license is GPL 2/3, therefore this version
	#   of the diagwl function is also licensed under GPL 2/3.
	#
	# MODIFICATIONS
	#   * The original used the R base graphics package.
	#     This version uses the R grid package.
	#   * The grid graphics functions return a "grob", or graphics object,
	#     rather than draw directly on the active device.
	#     Likewise, the diagwl function returns the final grob.
	#   * Added English translations to the original Spanish comments.
	#   * Added additional error checks.
	#   * Added optional display of English units along with SI units.
	#   * Viewport is only in npc units; lines units are not used. Helps with scaling plot.
	#
	diagwl.grid = function(
	    dat, est = NULL, alt = NULL, per = NULL, mlab = "en", unitsSI = TRUE,
	    pcol  = "#005ac8", tcol  = "#e81800",
	    pfcol = "#79e6e8", sfcol = "#09a0d1",
	    shem = FALSE, p3line = FALSE )
	{
	    # constants
	    isES <- mlab == tolower("es")	# lógico: es idioma Español? [logical: is language Spanish?]
	    units.npc     <- "snpc"
	    units.native  <- "native"
	    left          <- "left"	# left justification
	    right         <- "right"	# right justification
	    y.unit        <- 55		# Y position of units strings on each Y axis (temperature units)
	    y.meanMaxTemp <- 45		# Y position of mean daily max temp (temperature units)
	    y.meanMinTemp <- 5		# Y position of mean daily min temp (temperature units)
	    y.titleLine.1 <- 67		# Y position of top (first) title line (temperature units)
	    y.titleLine.2 <- 64		# Y position of 2nd title line (temperature units)
	    y.upperLimit  <- 50		# Y upper limit horizontal line (temperature units)
	    y.axisTempMax <- 60		# Y maximum of temperature axis (temperature units)
	    y.axisPrecMax <- 300	# Y maximum of precipitation axis (precipitation units)
	    label.offset.left  <- -0.7	# X position of left Y axis annotation
	    label.offset.right <- 13.8	# X position of right Y axis annotation
	    emptyLabel <- " "
	    fontsize <- 12		# default font size

	    UnitLabel.Temp <- function ( unitsSI )
	    {
		if ( unitsSI ) return("C") else return("F")
	    }
	    UnitLabel.Prec <- function ( unitsSI )
	    {
		if ( unitsSI ) return("mm") else return("in")
	    }
	    ValuesToLabels <- function ( values )
	    {
		labels <- as.character( values )
		labels[ is.na(labels) ] <- emptyLabel
		return(labels)
	    }
	    Units.ConvertLabels <- function ( labels, unitsSI, isTemp )
	    {
		# assume labels are currently in SI units
		if ( unitsSI )
		    return( labels )
		else # convert
		{
		    if ( isTemp )
			unitStrs <- c( "C", "F" )
		    else # precip
			unitStrs <- c( "mm", "in" )
		    values <- ConvertUnits( as.integer( labels ), unitStrs[1], unitStrs[2] )
		    values <- round( values, digits=1 )
		    return( ValuesToLabels( values ) )
		}
	    }
	    Units.ConvertValues <- function ( values, unitsSI, isTemp )
	    {
		# assume labels are currently in SI units
		if ( unitsSI )
		    return( values )
		else # convert
		{
		    if ( isTemp )
			unitStrs <- c( "C", "F" )
		    else # precip
			unitStrs <- c( "mm", "in" )
		    return( ConvertUnits( values, unitStrs[1], unitStrs[2] ) )
		}
	    }

	    unit.temp <- UnitLabel.Temp( unitsSI )
	    unit.prec <- UnitLabel.Prec( unitsSI )

	    # Comprobar los argumentos [check arguments]
	    if ( nrow(dat) != 4 || ncol(dat) != 12 )
	    {
		if ( isES )
		    stop( "Debe haber 4 filas de datos mensuales." )
		else
		    stop( "Argument dat must have 4 rows of data per month." )
	    }
	    dat <- as.matrix(dat)

	    # etiquetas de los meses [Months tags]
	    if ( isES )
		mlab <- c("E", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
	    else if (mlab == "en")
		mlab <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
	    else
		mlab <- c(1:12)		# etiquetas numéricas [Numeric labels]

	    if (shem)
	    {
		# Hemisferio sur: desplazar los datos medio año
		# southern hemisphere: shift data half year
		m1 <- dat[, 1:6]
		m2 <- dat[, 7:12]
		dat <- cbind(m2, m1)
		mlab <- c(mlab[7:12], mlab[1:6])
	    }

	    # precipitaciones medias mensuales [Monthly average rainfall]
	    p <- dat[1, ]
	    # temperaturas medias mensuales [Average monthly temperatures]
	    if ( nrow(dat) == 2 )
		tm <- dat[2, ]
	    else
		tm <- apply(dat[2:3, ], 2, mean)
	    pmax <- max(p)		# precipitación máxima [Maximum precipitation]
	    ymax <- y.axisTempMax 	# máxima ordenada por defecto [Maximum ordered by default]
	    if (pmax > y.axisPrecMax)
		ymax <- y.upperLimit + 10 * floor((pmax + 100)/200)
	    ymin <- min(-1.5, min(tm))	# mínima ordenada sin redondear [Minimum ordered without rounding]

	    # ejes [axes]
	    if (ymin < -1.5)
	    {
		ymin <- floor(ymin/10) * 10  # mínima ordenada redondeada [Minimum ordered with rounding]
		labT <- paste(ymin)
		labP <- emptyLabel
		if (ymin < -10)
		{
		    for (i in (ymin/10 + 1):-1)
		    {
			labT <- c(labT, i * 10)
			labP <- c(labP, emptyLabel)
		    }
		}
		# SI units
		labT <- c(labT, "0", "10", "20", "30", "40", "50", emptyLabel)
		labP <- c(labP, "0", "20", "40", "60", "80", "100", "300")
	    }
	    else
	    {
		# SI convert units
		labT <- c("0", "10", "20", "30", "40", "50", emptyLabel)
		labP <- c("0", "20", "40", "60", "80", "100", "300")
	    }
	    if (ymax > 60)
	    {
		for (i in 6:(ymax/10 - 1))
		{
		    labT <- c(labT, emptyLabel)
		    labP <- c(labP, 100 * (2 * i - 7))
		}
	    }
	    labT <- Units.ConvertLabels( labT, unitsSI, TRUE )
	    labP <- Units.ConvertLabels( labP, unitsSI, FALSE )

	    # axes values
	    x <- 1:12
	    x.range <- range(x)
	    x.scale <- 1:13 - 0.5
	    x.scale.range <- range(x.scale)
	    y.range <- c(ymin, ymax)
	    y.scale <- y.range + c(-0.05, 0.05) * diff(y.range)
	    # viewport
	    vp.diag <- grid::viewport(
		x = unit(0.18, units.npc), y = unit(0.05, units.npc),	# origin location in viewport
		width  = unit(0.65, units.npc),
		height = unit(0.85, units.npc),
		xscale = range(x.scale),
		yscale = y.scale,
		just = c("left", "bottom"),
		default.units = units.native,
		gp = gpar(fontsize=fontsize) )
#	    grid::pushViewport( vp.diag )

	    # draw axes and labels
	    lmin <- ymin	# mínima ordenada a rotular [minimum ordinate to label]
	    if (lmin == -1.5)
		lmin <- 0
	    # rótulo [label] left and right axes
	    tics.y <- ( (lmin/10):(ymax/10) ) * 10
	    # pg = list of grob containing the plot
	    pg <- gList(     yaxisGrob( at = tics.y, label = labT, gp = gpar(col = tcol), main = TRUE ) )
	    pg <- gList( pg, yaxisGrob( at = tics.y, label = labP, gp = gpar(col = pcol), main = FALSE ) )
	    # rótulo de las unidades [units labels]
	    pg <- gList(pg, textGrob( unit.temp[1], x = label.offset.left,  y = y.unit,
				      default.units=units.native,
				      just=right, gp = gpar(col = tcol) ) )
	    pg <- gList(pg, textGrob( unit.prec[1], x = label.offset.right, y = y.unit,
				      default.units=units.native,
				      just=left, gp = gpar(col = pcol) ) )
	    # horizonal line for upper limit
	    pg <- gList(pg, linesGrob( x = c( x.scale.range[1], x.scale.range[2] ),
				       y = c( y.upperLimit, y.upperLimit ),
				       default.units=units.native ) )
	    # rótulos meses: [Month labels]
	    # y.monthLabels <- -12.5	# Y position of month labels (native units) (temperature units)
	    y.monthLabels <- ymin - 2.5
	    pg <- gList(pg, textGrob( mlab, x, y.monthLabels, default.units=units.native ) )

	    # rótulos:
	    # nombre de la estación climatológica [name of the climatological station]
	    if ( !is.null(est) )
	    {
		if ( is.null(alt) )
		    pg <- gList(pg, textGrob( est, x = 0.5, y = y.titleLine.1, gp=gpar(fontsize=fontsize),
					      default.units=units.native, just=left ) )
		else # have altitude
		    pg <- gList(pg, textGrob( paste(est, " (", alt, " m)", sep = ""),
					      x = 1.0, y = y.titleLine.1,
					      default.units=units.native, just=left ) )
	    }
	    # período de promedios [period for averages]
	    if ( !is.null(per) )
		pg <- gList(pg, textGrob( per, x = 0.5, y = y.titleLine.2,
					  default.units=units.native, just=left ) )

	    # mean temperature label
	    tm.mean <- round(mean(tm * 10))/10
	    tm.mean <- round( Units.ConvertValues( tm.mean, unitsSI, TRUE ), digits=1 )
	    tm.mean <- ValuesToLabels( tm.mean )
	    tempStr <- paste( tm.mean, unit.temp[1] )
	    # mean precip label
	    pr.sum <- round( sum(p) )
	    pr.sum <- round( Units.ConvertValues( pr.sum, unitsSI, FALSE ), digits=1 )
	    pr.sum <- ValuesToLabels( pr.sum )
	    precStr <- paste( pr.sum, unit.prec[1] )
	    # add to grob
	    pg <- gList(pg, textGrob( tempStr, x =  8.5, y = y.titleLine.2,
				      default.units=units.native, just=left ) )
	    pg <- gList(pg, textGrob( precStr, x = 10.5, y = y.titleLine.2,
				      default.units=units.native, just=left ) )

	    # línea de precipitación suplementaria [supplementary precipitation line]
	    if (p3line)
	    {
		# línea adicional de precipitación a escala 1:3
		# Additional line of precipitation at scale 1:3
		yl3 <- c(p[12], p[1:12], p[1])/3
		yl3[yl3 > y.upperLimit] <- y.upperLimit
	    }

	    # precipitation line
	    x <- c( 0.5, 1:12, 12.5 )	# includes edges
	    if (pmax <= 100)
	    {
		xl <- x
		yl <- c(p[12], p[1:12], p[1])/2
		n2 <- 14
	    }
	    else
	    {
		# cambio de escala en prec. > 100 [change scale for prec > 100]
		# draw a curve for values below y.upperLimit; draw solid polygon above
		xl <- yl <- numeric(14) 		# line vertices (x, y)
		n2 <- 0					# index to line vertices
		xp <- yp <- numeric(15) 		# polygon vertices (x, y)
		n <- 0					# index to polygon vertices
		inPoly <- FALSE				# If T, a polygon has been started
		p2 <- c(p[12], p[1:12], p[1])		# dec, jan, feb, ..., dec, jan; length=14
		prec.upperFactor <- 0.05;		# (precip - 100) / 200 * 20 / 2

		# primer punto [first point]
		if ( p2[1] > 100 )		# dec above y.upperLimit?
		{
		    # points for polygon
		    # intersection of left edge line and line between 1st 2 points
		    m <- p2[2] - p2[1]			# divisor == 1
		    yi <- ( m * (x[1] - 0) + p2[1] ) / 2	# y at intersection
		    if ( yi > y.upperLimit )
		    {
			yi <- y.upperLimit + (yi * 2 - 100) * prec.upperFactor
			n <- n + 1
			xp[n] <- x[1]
			yp[n] <- yi
			inPoly <- TRUE
			# points for curve
			n2 <- n2 + 1
			xl[n2] <- x[1]
			yl[n2] <- y.upperLimit
		    }
		    else
		    {
			inPoly <- FALSE
			# points for curve
			n2 <- n2 + 1
			xl[n2] <- x[1]
			yl[n2] <- yi
		    }
#message("--- first point"); browser() # p2; xl[1:n2]; yl[1:n2]*2; inPoly; xp[1:n]; yp[1:n]*2
		}
		else				# left edge of diagram
		{
		    n2 <- n2 + 1
		    xl[n2] <- x[1]
		    yl[n2] <- ( p2[1] + ( p2[2] - p2[1] ) * 0.5 )  / 2	# at intersection of left edge
		}

		# remaining points
		for (i in 2:14)  		# i == next point in p2 and x
		{
		    if ( inPoly )			# continuing polygon
		    {
			closePoly <- FALSE
			if ( i < 14 ) # interior
			{
			    if ( p2[i] > 100 )	# continue polygon
			    {
				# polygon point
				n <- n + 1
				xp[n] <- x[i]
				yp[n] <- y.upperLimit + (p2[i] - 100) * prec.upperFactor
			    }
			    else # curve intersects y.upperLimit downward
			    {
				yi <- y.upperLimit
				xi <- abs( (100 - p2[i - 1]) / (p2[i] - p2[i - 1]) )
				if ( i > 2 )
				    xi <- x[i - 1] + xi
				else if ( xi < x[1] ) # before left edge?
				{
				    m <- p2[2] - p2[1]				# divisor == 1
				    xi <- x[1]					# x at  right edge
				    yi <- ( m * (xi - 0) + p2[1] ) / 2	# y at intersection
				}
				# close polygon
				n <- n + 1
				xp[n] <- xi
				yp[n] <- y.upperLimit
				n <- n + 1
				xp[n] <- xp[1]
				yp[n] <- y.upperLimit
				closePoly <- TRUE
				# points for curve
				# y.upperLimit intersection
				n2 <- n2 + 1
				xl[n2] <- xi
				yl[n2] <- y.upperLimit
				# actual point
				n2 <- n2 + 1
				xl[n2] <- x[i]
				yl[n2] <- p2[i] / 2
				# poly closes below
#message("--- inPoly p2[i] <= 100"); browser() # i; p2; xl[1:n2]; yl[1:n2]*2; xp[1:n]; yp[1:n]*2
			    }
			}
			else # at right edge
			{
			    m <- p2[14] - p2[13]	# divisor == 1
			    if ( p2[i] <= 100 )
				yi <- y.upperLimit				# y at intersection
			    else
				yi <- ( m * (x[14] - x[13]) + p2[14] ) / 2	# y at intersection
			    xi <- x[13] + ( yi * 2 - p2[13] ) / m		# x at y.upperLimit
			    # points for polygon
			    # line from last poly point to y.upperLimit; need X at intersection
			    if ( xi > x[14] )	# restrict to x[14]
			    {
				xi <- x[14]					# x at  right edge
				yi <- ( m * (x[14] - x[13]) + p2[13] ) / 2	# y at intersection
			    }
			    # right edge polygon bottom
			    if ( yi > y.upperLimit )	# on right edge and > y.upperLimit?
			    {
				n <- n + 1
				xp[n] <- xi
				yp[n] <- y.upperLimit + (yi * 2 - 100) * prec.upperFactor
				n <- n + 1
				xp[n] <- x[14]
				yp[n] <- y.upperLimit
			    }
			    else
			    {
				n <- n + 1
				xp[n] <- xi
				yp[n] <- yi
			    }
			    # close polygon
			    n <- n + 1
			    xp[n] <- xp[1]
			    yp[n] <- y.upperLimit
			    closePoly <- TRUE

#message("--- in poly, right edge"); browser() # i; inPoly; p2; xp[1:n]; yp[1:n]*2

			    # points for curve
			    # y.upperLimit and right edge intersection
			    if ( xi < x[14] )
			    {
				n2 <- n2 + 1			# x at y.upperLimit
				xl[n2] <- xi
				yl[n2] <- y.upperLimit
			    }
			    else
			    {
				n2 <- n2 + 1
				xl[n2] <- x[14]
				yl[n2] <- y.upperLimit
			    }
			}

			if ( closePoly )	# close polygon
			{
#message("--- in poly, closing poly"); browser() # i; p2; xp[1:n]; yp[1:n]*2

			    pg <- gList(pg, polygonGrob( xp[1:n], yp[1:n], default.units=units.native,
							 gp = gpar(col = pcol, fill=pcol) ) )
			    xp <- yp <- numeric(15)		# re-init polygon vertices
			    n <- 0				# re-init index
			    inPoly <- FALSE
			}
		    }
		    else # not in polygon
		    {
			if ( i < 14 )			# interior
			{
			    if ( p2[i] > 100 )		# new polygon
			    {
				inPoly <- TRUE
				# y.upperLimit intersection
				xAtUpperLimit <- abs( (100 - p2[i - 1]) / (p2[i] - p2[i - 1]) )
				if ( i > 2 )
				    xAtUpperLimit <- x[i - 1] + xAtUpperLimit
				# points for polygon
				#   y.upperLimit intersection
				n <- n + 1
				xp[n] <- xAtUpperLimit
				yp[n] <- y.upperLimit
				#   actual point
				n <- n + 1
				xp[n] <- x[i]
				yp[n] <- y.upperLimit + (p2[i] - 100) * prec.upperFactor
				# points for curve
				#   y.upperLimit intersection
				n2 <- n2 + 1
				xl[n2] <- xAtUpperLimit
				yl[n2] <- y.upperLimit
			    }
			    else # p2[i] <= 100
			    {
				# points for curve
				n2 <- n2 + 1
				xl[n2] <- x[i]
				yl[n2] <- p2[i] / 2
#message("--- not in polygon; interior"); browser() # i; p2; xl[1:n2]; yl[1:n2]*2
			    }
			}
#			if ( i == 14 )	# at right edge and !inPoly
#			{
#			    n2 <- n2 + 1
#			    xl[n2] <- x[14]
#			    yl[n2] <- ( p2[13] + ( p2[14] - p2[13] ) * 0.5 ) / 2	# at right edge
#			    if ( yl[n2] > y.upperLimit )
#				yl[n2] <- y.upperLimit
#message("--- not in polygon; at right edge"); browser() # i; p2; xl[1:n2]; yl[1:n2]*2
#			}
		    }
		}

		if ( inPoly )			# cerrar último polígono [Close last polygon]
		{
		    n <- n + 1
		    xp[n] <- x[14]
		    yp[n] <- y.upperLimit
		    n <- n + 1
		    xp[n] <- xp[1]
		    yp[n] <- y.upperLimit
		    # polygon(xp[1:n], yp[1:n], col = pcol, border = pcol)
		    pg <- gList(pg, polygonGrob( xp[1:n], yp[1:n], default.units=units.native,
						 gp = gpar(col = pcol, fill=pcol) ) )
		    inPoly <- FALSE
		    # points for curve
		    if ( xl[n2] != x[14] )
		    {
			n2 <- n2 + 1
			xl[n2] <- x[14]
			yl[n2] <- y.upperLimit
		    }
		}
		else if ( xl[n2] != x[14] )	# points for curve
		{
		    n2 <- n2 + 1
		    xl[n2] <- x[14]
		    yl[n2] <- ( p2[13] + ( p2[14] - p2[13] ) * 0.5 ) / 2	# at right edge
		    if ( yl[n2] > y.upperLimit )
			yl[n2] <- y.upperLimit
		}
		xl <- xl[1:n2]
		yl <- yl[1:n2]
#browser()
	    }
	    # tramas: [plots]
	    numPts <- 66
	    pi <- approx(xl[1:n2], yl[1:n2], n = numPts)$y
	    ti <- approx(x, c(tm[12], tm[1:12], tm[1]), n = numPts)$y
	    ti[ti < 0] <- 0  # no poner tramas por debajo del cero [Do not put frames below zero]
	    d <- pi - ti
	    # xi <- (1:numPts)/5 - 0.7
	    xi <- seq( min(x), max(x), length.out=numPts )
	    # periodo húmedo [wet period plot]
	    xw <- subset(xi, d > 0)
	    y1 <- subset(pi, d > 0)
	    y2 <- subset(ti, d > 0)
	    if ( length(xw) > 2 )
	    {
		# vertical lines for wet period region
		# remove the leftmost and rightmost lines so they do not overwrite the axes
		xw <- xw[-1]; xw <- xw[-length(xw)]
		y1 <- y1[-1]; y1 <- y1[-length(y1)]
		y2 <- y2[-1]; y2 <- y2[-length(y2)]
		# plot
		pg <- gList(pg, segmentsGrob( xw, y1, xw, y2, default.units=units.native,
						gp = gpar(col = pcol, lty = 1, lwd = 1) ) )
	    }
	    # periodo seco [dry period plot]
	    xw <- subset(xi, d < 0)
	    y1 <- subset(pi, d < 0)
	    y2 <- subset(ti, d < 0)
	    if (length(xw) > 0)
	    {
		pg <- gList(pg, segmentsGrob( xw, y1, xw, y2, default.units=units.native,
					 gp = gpar(col = tcol, lty = 3, lwd = 2) ) )
	    }
	    # heladas seguras [definitely frost plot]
	    for (i in 1:12)
	    {
		if (dat[3, i] <= 0)
		{
		    pg <- gList(pg, rectGrob( i, -0.75, 1, 1.5, default.units=units.native,
					 gp = gpar(col = 0, fill = sfcol ) ) )
		}
	    }
	    # heladas probables [likely frost plot]
	    for (i in 1:12)
	    {
		if (dat[4, i] <= 0 && dat[3, i] > 0)
		{
		    pg <- gList(pg, rectGrob( i, -0.75, 1, 1.5, default.units=units.native,
					 gp = gpar(col = 0, fill = pfcol ) ) )
		}
	    }

	    # curvas de P y T: [prec and temp curves]
	    pg <- gList(pg, linesGrob( xl[1:n2], yl[1:n2], default.units=units.native,
					gp = gpar(col = pcol, lwd = 2) ) )
	    if (p3line)
		pg <- gList(pg, linesGrob( x, yl3, default.units=units.native ) )
	    pg <- gList(pg, linesGrob( x, c(tm[12], tm[1:12], tm[1]), default.units=units.native,
					gp = gpar(col = tcol, lwd = 2) ) )

	    # media de las máximas del mes más cálido, en el lado izquierdo
	    # Average of the maximums of the warmest month, on the left side
	    tempMax <- Units.ConvertValues( max(as.matrix(dat[2, ])), unitsSI, TRUE )
	    labels <- formatC(tempMax, digits = 1, format = "f")
	    pg <- gList(pg, textGrob( labels, label.offset.left + 0.02, y.meanMaxTemp,
					default.units=units.native, just=right ) )

	    # media de las mínimas del mes más frío, en el lado izquierdo
	    # Average of the minimums of the coldest month, on the left side
	    tempMin <- Units.ConvertValues( min(as.matrix(dat[3, ])), unitsSI, TRUE )
	    labels <- formatC(tempMin, digits = 1, format = "f")
	    pg <- gList(pg, textGrob( labels, label.offset.left + 0.02, y.meanMinTemp,
					default.units=units.native, just=right ) )

	    # marcar límites de los meses en la barra horizontal de heladas:
	    # Mark month limits in the horizontal frost bar
	    for (i in x.scale[2:12])
		pg <- gList(pg, segmentsGrob( i, 0, i, -1.5, default.units=units.native ) )
	    # líneas horizontales alrededor de la barra de heladas
	    # horizontal lines around frost bar
	    pg <- gList(pg, linesGrob( x = c( x.scale.range[1], x.scale.range[2] ), y = c( 0, 0 ),
				  default.units=units.native ) )
	    pg <- gList(pg, linesGrob( x = c( x.scale.range[1], x.scale.range[2] ), y = c( -1.5, -1.5 ),
				  default.units=units.native ) )

	    # reset old.par (restablecemos parámetros gráficos anteriores):
#	    pg <- gTree( vp=vp.diag, name="WLDiagram", children=pg )
	    pg <- grobTree( pg, vp=vp.diag, name="WLDiagram" )
#	    grid::popViewport()
	    return(pg)
	},

	#' @export
	Plot = function( graphConfig )
	{
	    "\\cr
	    Draw a Walter and Lieth (1960) diagram.
	    \\cr
	    Based upon the function diagwl in the climatol R package.

	    \\strong{Arguments}
	    \\describe{
	      \\item{ graphConfig }{ \\cr list: plot configuration values }
	      \\item{ return }{ \\cr (invisibly) A plot object; a 'grob' produced by package 'grid' }
	      }
	    "

	    periodStr <- PairsToString( yearRange )
	    # following potentially can run over mean temp+precip text
	    periodStr <- paste( rcp, periodStr )
	    plot.grob <- diagwl.grid( df, est=regionName, per=periodStr, unitsSI=isUnitsSI )
	    attr( plot.grob, "numPlots" ) <- 1
	    attr( plot.grob, "ncol" ) <- 1
	    attr( plot.grob, "nrow" ) <- 1
	    return( plot.grob )
	}

    ) # methods
) # setRefClass

#--- END ---
