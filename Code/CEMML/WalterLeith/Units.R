#------------------------------------------------------------------------------------------------------------
# file:         Units.R
# project:      ClimatePrimers
# organization: Natural Resource Ecology Laboratory at Colorado State University.
#               Previously: North Central Climate Adaptation Science Center (nccsc.colostate.edu)
# description:  Functions that manage data units.
# author:       Thomas E. Hilinski <tom.hilinski@colostate.edu>
#               based upon original function by Marian Talbert <usgs.gov>
# license:      See LICENSE.md or https://unlicense.org
#------------------------------------------------------------------------------------------------------------

# About units:
# Units management should be enhanced to handle various mutable characteristics
# of units:
# * Some units have time-dependencies (e.g., mm/day).
# * Some units will be associated with aggregation (e.g. mean temperature).
# * Precipitation is usually summed for aggregation, but sometimes averaged.
# * Temperature is usually averaged.
# A Units class would allow attaching units w/units metadata to every
# container (e.g, ClimateTS, ClimateMapTS, data.frame, and list).

# TODO: Make classes: UnitsBase, UnitsSI, UnitsEnglish to contain these functions

#' Returns a list of known temperature and precipitation unit strings
#' in their abbreviated form, or searches known units for a match to the argument.
#' @param unitStr	Optional unit string; if supplied, known units are searched for a match;
#'			if found, returns TRUE else FALSE if not.
#' @return	character vector if unitStr is NULL, or TRUE found the unitStr, or FALSE if not found.
#' @export
#' @concept units
#' @family units functions
UnitsKnown <- function ( unitStr=NULL )
{
    units.list <- list(
	temperature   = c( "K", "C", "F", "dC", "dF" ),
	precipitation = c( "mm", "cm", "in", "mm_day", "cm_day", "in_day", "kg m-2 s-1" ),
	elevation     = c( "feet", "meters" ),
	area          = c( "km2", "miles2" ) )
    if ( is.null(unitStr) )
	return( units.list )
    else
    {
	for ( units in units.list )
	{
	    if ( unitStr %in% units )
		return(TRUE)
	}
	return(FALSE)
    }
}

Catalog.Units.Temp <- function()	# match UnitsKnown()$temperature
{
    return( data.frame(
		units = UnitsKnown()$temperature,
		description = c( "degrees Kelvin",
				 "degrees Celsius",
				 "degrees Fahrenheit",
				 "change in degrees Celsius",
				 "change in degrees Fahrenheit" ),
		stringsAsFactors=FALSE ) )
}

Catalog.Units.Precip <- function()	# match UnitsKnown()$precipitation
{
    return( data.frame(
		units = UnitsKnown()$precipitation,
		description = c( "millimeters",
				 "centimeters",
				 "inches",
				 "millimeters per day",
				 "centimeters per day",
				 "inches per day",
				 "kg H2O m-2 s-1" ),
		stringsAsFactors=FALSE ) )
}

Catalog.Units.Elevation <- function()	# match UnitsKnown()$elevation
{
    return( data.frame(
		units = UnitsKnown()$elevation,
		description = c( "feet",
				 "meters" ),
		stringsAsFactors=FALSE ) )
}

Catalog.Units.Area <- function()	# match UnitsKnown()$area
{
    return( data.frame(
		units = UnitsKnown()$area,
		description = c( "square kilometers",
				 "square miles" ),
		stringsAsFactors=FALSE ) )
}

#' Returns a catalog of units and descriptions.
#' @param asList	logical: if TRUE, return a list of 2 data.frames;
#'			if FALSE, return a single data.frame;
#'			default is TRUE.
#' @return	list of two data.frames; temperature and precipitation
#' @export
#' @concept units
#' @family units functions
Catalog.Units <- function( asList=TRUE )
{
    temp.df <- Catalog.Units.Temp()
    prec.df <- Catalog.Units.Precip()
    elev.df <- Catalog.Units.Elevation()
    area.df <- Catalog.Units.Area()
    if ( asList )
    {
	catalog <- list( temperature = temp.df, precipitation = prec.df,
			 elevation = elev.df, area = area.df )
	return( catalog )
    }
    else # one data.frame, 4 columns
    {
	catalog <- DF.Match.NRows( temp.df, prec.df )		# returns a list
	catalog.df <- cbind( catalog[[1]], catalog[[2]] )
	names(catalog.df)[1] <- "temperature"
	names(catalog.df)[3] <- "precipitation"
	catalog <- DF.Match.NRows( catalog.df, elev.df )	# returns a list
	catalog.df <- cbind( catalog.df, catalog[[2]] )
	names(catalog.df)[5] <- "elevation"
	catalog <- DF.Match.NRows( catalog.df, area.df )	# returns a list
	catalog.df <- cbind( catalog.df, catalog[[2]] )
	names(catalog.df)[7] <- "area"
	return( catalog.df )
    }
}

IsTempUnit <- function ( unitStr )
{
    return( toupper(unitStr) %in% toupper( UnitsKnown()$temperature ) )
}

IsPrecipUnit <- function ( unitStr )
{
    return( tolower(unitStr) %in% UnitsKnown()$precipitation )
}

IsElevUnit <- function ( unitStr )
{
    return( tolower(unitStr) %in% UnitsKnown()$elevation )
}

IsAreaUnit <- function ( unitStr )
{
    return( tolower(unitStr) %in% UnitsKnown()$area )
}

# Returns an index into the precip or temp catalogs or known units.
# example:   Catalog.Units.Precip()[ Units.Index("mm_day"), ]$description
Units.Index <- function( unitStr )
{
    i <- NULL
    if ( IsPrecipUnit(unitStr) )
	i <- which( unitStr == UnitsKnown()$precipitation )
    if ( IsTempUnit(unitStr) )
	i <- which( unitStr == UnitsKnown()$temperature )
    if ( IsElevUnit(unitStr) )
	i <- which( unitStr == UnitsKnown()$elevation )
    if ( IsAreaUnit(unitStr) )
	i <- which( unitStr == UnitsKnown()$area )
    if ( length(i) > 0 )
	return( i )
    else
	return( NULL )
}

#' List of unit strings for temperature and precipitation units used internally.
#' @param isMonthly	if TRUE, units are for a monthly time step dataset, else a daily dataset;
#'			default is TRUE (monthly)
#' @return	character vector
#' @export
#' @concept units
#' @family units functions
InternalUnits <- function ( isMonthly = TRUE )
{
    if ( isMonthly )
	return( list(prec="mm", temp="C") )
    else # daily
	return( list(prec="mm_day", temp="C") )
}

#' Is the unit string an SI unit?
#' @param unitStr	string from list of UnitsKnown()
#' @return		logical; TRUE if argument is a known SI unit, else FALSE
#' @export
#' @concept units
#' @family units functions
IsUnitsSI <- function ( unitStr )
{
    if ( UnitsKnown(unitStr) )
	return( unitStr %in% c( "K", "C", "dC", "mm", "cm", "mm_day", "cm_day", "meters", "km2" ) )
    else
	return( FALSE )
}

IsChangeUnit <- function( unit )
{
    if ( is.na(unit) )
	return( FALSE )
    return( unit == "dC" || unit == "dF" || unit == "dP" )
}

ToShortUnitName <- function( unitStr )
{
    in.long <- "inches"
    in.short <- "in"
    if ( unitStr == in.long )
	unitStr <- in.short
    return( unitStr )
}

#' Unit conversion for one or more values.
#' @param x		Numeric value or vector
#' @param fromUnits	string: units of input values
#' @param toUnits	string: units of output values
#' @param month		Optional month number (1-12) if converting from daily to monthly units;
#' @param isLeapYear	if TRUE, month is in a leap year; default is FALSE
#'			if supplied, should be the same length as x.
#' @param isDaily	logical; if TRUE, x indicates daily data
#' @return		Numeric value or vector of the same length as x, or x if unable to convert units.
#' @export
#' @concept units
#' @family units functions
ConvertUnits <- function ( x, fromUnits, toUnits, month=NULL, isLeapYear=FALSE, isDaily=FALSE )
{
    if ( IsEmpty(x) )
    {
	warning("Argument vector x is empty.")
	return(x)
    }
    if ( fromUnits == toUnits )		# same? then all done
    {
	attr( x, "units" ) <- fromUnits
	return(x)
    }

    fromUnits <- ToShortUnitName( fromUnits )
    toUnits   <- ToShortUnitName( toUnits )

    if ( is.numeric(month) )
    {
	stopifnot( month >= 1 && month <= 12 )
	year <- ifelse(isLeapYear, 2000, 2001)
	numDays <- DaysInMonth( month, year, isLeapYear )
    }
    else	# known state
    {
	if ( isLeapYear )
	    numDays <- 30.50
	else
	    numDays <- 30.41667
    }
    if ( fromUnits == "mm" && isDaily )
	fromUnits <- "mm_day"

    fromStr <- gsub(" ", "_", fromUnits )
    fromStr <- gsub("-", "_", fromStr )
    toStr <- gsub(" ", "_", toUnits )
    toStr <- sub( "day-1", "day", toStr )
    unitsConvertStr <- paste( toupper(fromStr), toupper(toStr), sep = "to" )

    if ( fromStr != toStr )
    {
	result <- switch(
		    unitsConvertStr,
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
		    IN_DAYtoIN   = x * numDays,
		    IN_DAYtoCM   = x * 2.54 * numDays,
		    IN_DAYtoMM   = x * 25.4 * numDays,
		    MM_DAYtoMM   = x * numDays,			# mm/month
		    MM_DAYtoCM   = x * 0.1 * numDays,		# cm/month
		    MM_DAYtoIN   = x * numDays * 0.0393701,	# inches/month
		    MMtoCM       = x * 0.1,
		    CMtoMM       = x * 10.0,
		    CM_DAYtoMM   = x * 10.0 * numDays,		# mm/month
		    CM_DAYtoCM   = x * numDays,			# cm/month
		    CM_DAYtoIN   = x * numDays * 0.3937008,	# inches/month
		    {
			warning( "Unknown units convertion: ", fromUnits, " to ", toUnits )
			x	# return unaltered input values
		    })
	if ( !is.null(result) )
	    attr( result, "units" ) <- toStr
    }
    else
    {
	result <- x
	attr( result, "units" ) <- toStr
    }

    if ( length(result) > 1 )
	return( as.vector(result) )
    else
	return(result)
}

# Convert units in a data.frame.
# columns == climate variable, e.g.
#    > job.env$stats$means.list$historical
#             Precip      Tmin       Tmax        Temp
#      mean 232.8363 8.5806941 20.6843997 14.63254692
#      sd   105.5527 0.2094648  0.2068356  0.05085904
# requires attribute "units" == vector of unit strings, one per column; e.g.,
#    > as.character( attributes(job.env$stats$means.list$historical)$units )
#    [1] "mm_day" "C"      "C"      "C"
# @param df		data.frame as described above
# @param units.prec	string: destination unit string for precipitation
# @param units.temp	string: destination unit string for temperature
# @param digits		integer: rounds the values to the specified number of decimal places; default is 2.
# @return		data.frame with converted values; attribute "units" contains a vector of units
ConvertUnits.DF <- function( df, units.prec, units.temp, digits=2 )
{
    units <- attr( df, "units" )
    stopifnot( !is.null( units ) )
    stopifnot( ncol(df) == length(units) )
    stopifnot( UnitsKnown( units.prec ) )
    stopifnot( UnitsKnown( units.temp ) )
    digits <- as.integer( digits )

    isList <- FALSE
    if ( is.list(units) )
    {
	isList <- TRUE
	units <- unlist( units )
    }
    for ( i in 1:ncol(df) )
    {
	if ( IsPrecipUnit( units[i] ) )
	{
	    df[,i] <- ConvertUnits( df[,i], units[i], units.prec )
	    units[i] <- units.prec
	}
	else if ( !is.na( units[i] ) )
	{
	    unit.out <- units.temp
	    if ( IsChangeUnit( units[i] ) )
		unit.out <- ConvertUnits.ToChange( unit.out )
	    for ( j in 1:nrow(df) )
	    {
		if ( row.names(df)[j] == "sd" )		# always is a temp change unit
		{
		    if ( !is.na(df[j,i]) )
			df[j,i] <- ConvertUnits( df[j,i],
						 ConvertUnits.ToChange( units[i] ),
						 ConvertUnits.ToChange( unit.out ) )
		}
		else					# maybe is a temp change unit
		    df[j,i] <- ConvertUnits( df[j,i], units[i], unit.out )
	    }
	    units[i] <- unit.out
	}
	df[,i] <- round( df[,i], digits )
    }
    if ( isList )
	units <- as.list( units )
    attr( df, "units" ) <- units
    return( df )
}

# Convert units in a data.frame according to column name.
# columns == climate variable, e.g.
#    > job.env$stats$means.list$historical
#             Precip      Tmin       Tmax        Temp
#      mean 232.8363 8.5806941 20.6843997 14.63254692
#      sd   105.5527 0.2094648  0.2068356  0.05085904
# @param df		data.frame as described above
# @param units.prec.in	string: input unit string for precipitation
# @param units.prec.out	string: output unit string for precipitation
# @param units.temp.in	string: input unit string for temperature
# @param units.temp.out	string: output unit string for temperature
# @param digits		integer: rounds the values to the specified number of decimal places; default is 2.
# @return		data.frame with converted values; attributes "units.precip" and "units.temp"
ConvertUnits.DF.ByColVar <- function(
    df, units.prec.in, units.prec.out, units.temp.in, units.temp.out, digits=2 )
{
    iP <- which( startsWith( names(df), "P" ) )		# precip
    iT <- which( startsWith( names(df), "T" ) )		# temp, tmin, tmax
    iA <- which( startsWith( names(df), "A" ) )		# atmn
    iT <- c( iT, iA )

    TSInfo.list <- attributes(df)$TSInfo.list

    df[,iP] <- ConvertUnits( df[,iP], units.prec.in, units.prec.out )
    df[,iP] <- round( df[,iP], digits )
    df[,iT] <- ConvertUnits( df[,iT], units.temp.in, units.temp.out )
    df[,iT] <- round( df[,iT], digits )
    attr( df, "units.precip" ) <- units.prec.out
    attr( df, "units.temp" )   <- units.temp.out

    if ( !is.null( TSInfo.list ) )
    {
	# TODO: change units in TSInfo objects
	stopifnot( all( names(df)[-1] == names(TSInfo.list) ) )
	iP <- iP - 1	# df cols start at col 2
	for ( i in iP )
	    TSInfo.list[[i]]@units <- units.prec.out
	iT <- iT - 1
	for ( i in iT )
	    TSInfo.list[[i]]@units <- units.temp.out
    }
    attributes(df)$TSInfo.list <- TSInfo.list
    return( df )
}

# Convert a temperature unit to a change-in-temperature unit.
ConvertUnits.ToChange <- function( units )
{
    stopifnot( all( sapply( units, UnitsKnown ) ) )

    isList <- FALSE
    if ( is.list(units) )
    {
	isList <- TRUE
	units <- unlist( units )
    }
    for ( i in 1:length(units) )
    {
	if ( IsTempUnit( units[i] ) )
	{
	    if ( units[i] == "C" )
		units[i] <- "dC"
	    else if ( units[i] == "F" )
		units[i] <- "dF"
	}
#	else if ( IsPrecipUnit( units[i] ) )
#	{
#	}
    }
    if ( isList )
	units <- as.list( units )
    return( units )
}

# Convert a units string from the original to one having the frequency specified.
# @param units		string: original units string
# @param dataFreq	integer: annual frequency of data (e.g, 12 = monthly, 365 = daily).
# @return	a new units string
ConvertUnitsStr <- function( units, dataFreq=0 )
{
    # check for the known units with time in it
    dayStr <- "_day"
    if ( dataFreq != 365 )
	return( sub( dayStr, "", units ) )
    return( units )	# no change
}

# Convert a list of value-unit pairs to the precipitation and temperature units specified.
ConvertValueListUnits <- function( values.list, prec.unit, temp.unit, isDaily=FALSE )
{
    for ( i in 1:length(values.list) )
    {
	vu <- values.list[[i]]	# list( value, unit )
	if ( IsPrecipUnit(vu$unit) )
	    newUnit <- prec.unit
	else if ( IsTempUnit(vu$unit) )
	    newUnit <- temp.unit
	else
	    stop( "Unknown unit: ", vu$unit )
	newValue <- ConvertUnits( vu$value, vu$unit, newUnit, isDaily=isDaily )
	if ( is.null(newValue) )
	    stop( "Unknown unit in unit conversion." )
	vu$value <- as.numeric( newValue )
	if ( is.null(attributes( newValue )$units) )
	    vu$unit  <- newUnit
	else
	    vu$unit  <- attributes( newValue )$units
	values.list[[i]] <- vu
    }
    return( values.list )
}

#' Get an object's unit attribute or member value.
#' @param object	Any object with an attribute 'unit' or 'units',
#'			or a class or list with member 'unit' or 'units'.
#' @param verbose	logical: if TRUE, provide warning and error messages;
#'			if FALSE, return silently;
#'			default is TRUE.
#' @return	A string specifying the unit for the object. If it does not exist, returns NULL.
#' @export
GetUnitStr <- function( object, verbose=TRUE )
{
    unit <- attr( object, "unit" )
    if ( !is.null(unit) )
	return( unit )
    unit <- attr( object, "units" )
    if ( !is.null(unit) )
	return( unit )
    if ( is( object, "refClass" ) )
    {
	objNames <- names( object$getRefClass()$fields() )
	i <- which( startsWith( objNames, "unit" ) )
	if ( length(i) > 0 )
	    return( object$field( objNames[i] ) )
    }
    else if ( isS4(object) )
    {
	objNames <- names( getSlots( class(object) ) )
	i <- which( startsWith( objNames, "unit" ) )
	if ( length(i) > 0 )
	    return( slot( object, objNames[i] ) )
    }
    else if ( is.list(object) )
    {
	i <- which( startsWith( names(object), "unit" ) )
	if ( length(i) > 0 )
	    return( object[[ names(object)[i] ]] )
    }
    else if ( verbose )
	warning( "Unrecognized object type." )
    return( NULL )
}


# end
