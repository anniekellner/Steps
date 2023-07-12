#------------------------------------------------------------------------------------------------------------
# file:         ClimateVars.R
# project:      ClimatePrimers
# organization: Natural Resource Ecology Laboratory at Colorado State University.
#               Previously: North Central Climate Adaptation Science Center (nccsc.colostate.edu)
# description:  Names and labels for climate variables and RCPs.
# author:       Thomas E. Hilinski <tom.hilinski@colostate.edu>
# license:      See LICENSE.md or https://unlicense.org
#------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------
#	climate variable names
#------------------------------------------------------------------------------------------------------------

#' Given a data variable name from a dataset, return an equivalent standardized variable name.
#' @param varName	string: data variable name; default is NULL.
#' @param verbose	if TRUE display warning message when appropriate; default is TRUE.
#' @return		string: standard variable name.
#'			If varName is NULL, return a vector of variable names.
#'			If not recognized, return varName.
#' @export
#' @concept	variables
#' @family climate variables functions
Var.StandardName <- function ( varName = NULL, verbose=TRUE )
{
    stdVarNames <- c( "Temp", "Tmax", "Tmin", "Precip", "ATmn" )
    searchable  <- c( "tavg", "tmax", "tmin", "prec",   "atmn" )

    if ( is.null(varName) )
	return( stdVarNames )

    if ( all( varName %in% stdVarNames ) )
	return( varName )

    varNameLC <- tolower( varName )
    for ( i in 1:length( stdVarNames ) )
    {
	svn <- stdVarNames[i]
	svns.known <- tolower( CollectVarNamesFromData( svn ) )
	svns.known <- c( svns.known, searchable[i] )
	if ( varNameLC %in% svns.known )
	    return( svn )
    }

#    if ( varNameLC %in% c( "temp", "tas", "tmean", "tdmean", "tavg" ) )
#        return( stdVarNames[1] )
#    if ( varNameLC %in% c( "tmx", "tmax", "tasmax" ) )
#        return( stdVarNames[2] )
#    if ( varNameLC %in% c( "tmn", "tmin", "tasmin" ) )
#        return( stdVarNames[3] )
#    if ( varNameLC %in% c( "pr", "ppt", "precip", "prec", "prc", "pre", "prcp" ) )
#        return( stdVarNames[4] )
#    if ( varNameLC %in% c("atmn") )
#        return( stdVarNames[5] )

    if ( !IsEmpty(varName) && verbose )
	warning( "Unrecognized variable name: ", varName )
    return( varName )
}

#' Given a data variable name, return an abbreviated variable name.
#' @param varName	string: data variable name; default is NULL
#' @param timeFrequency	string: frequency of data; valid values are ValidFrequencies(); default is NULL
#' @param verbose	if TRUE display warning message when appropriate; default is TRUE.
#' @return		string: abbreviated variable name.
#'			If varName is NULL, return a vector of long variable names.
#'			If not recognized, return varName.
#' @export
#' @concept	variables
#' @family climate variables functions
Var.ShortName <- function ( varName = NULL, timeFrequency = NULL, verbose=TRUE )
{
    abbrevNames <- c( "Avg Temp", "Max Temp", "Min Temp", "Precip", "Abs Min Temp" )

    if ( is.null(varName) )
	return( abbrevNames )

    varName <- Var.StandardName( varName, verbose )
    i <- which( Var.StandardName() %in% varName )
    if ( length(i) == 0 )
	return( varName )

    if ( !is.null(timeFrequency) )
    {
	freqStr <- timeFrequency
	substring(freqStr, 1) <- toupper(substring(freqStr, 1, 1))
	abbrevName <- paste( freqStr, abbrevNames[i] )
    }
    else
	return( abbrevNames[i] )
}

#' Given a data variable name, return a descriptive variable name.
#' @param varName	string: data variable name; default is NULL
#' @param timeFrequency	string: frequency of data; valid values are ValidFrequencies()
#' @param verbose	if TRUE display warning message when appropriate; default is TRUE.
#' @return		string: a descriptive variable name; if no match, returns varName.
#'			If varName is NULL, return a vector of long variable names.
#'			If not recognized, return varName.
#' @export
#' @concept	variables
#' @family climate variables functions
Var.LongName <- function ( varName = NULL, timeFrequency = NULL, verbose=TRUE )
{
    longNames <- c( "Average Temperature", "Maximum Temperature", "Minimum Temperature",
		    "Precipitation", "Absolute Minimum Temperature" )

    if ( is.null(varName) )
	return( longNames )

    varName <- Var.StandardName( varName, verbose )
    i <- which( Var.StandardName() %in% varName )
    if ( length(i) == 0 )
	return( varName )

    if ( !is.null(timeFrequency) )
    {
	freqStr <- timeFrequency
	substring(freqStr, 1) <- toupper(substring(freqStr, 1, 1))	# capitalize
	longName <- paste( freqStr, longNames[i] )
    }
    else
	return( longNames[i] )
}

# Get a list of RCP names that can be used as item and column names.
# currently not used
#RCP.InternalNames <- function ()
#{
#    return( "historical", "rcp26", "rcp45", "rcp60", "rcp85" )
#}

#' Check if a variable name is a temperature variable.
#' @param varName	string: data variable name
#' @return		logical: TRUE if varName appears to be a temperature variable, else FALSE.
#' @export
#' @concept	variables
#' @family climate variables functions
IsTempVar <- function ( varName )
{
    # quick version -- does not check entire string
    return( toupper( substring(varName, 1, 1) ) == "T" )
}

#' Check if a variable name is a precipation variable.
#' @param varName	string: data variable name
#' @return		logical: TRUE if varName appears to be a precipation variable, else FALSE.
#' @export
#' @concept	variables
#' @family climate variables functions
IsPrecipVar <- function ( varName )
{
    # quick version -- does not check entire string
    return( toupper( substring(varName, 1, 1) ) == "P" )
}

#' Given a data variable name, return a standardized variable name representing a difference value.
#' @param varName	string: data variable name
#' @return		string: difference variable name; if varName is not recognized, return varName.
#' @export
#' @concept	variables
#' @family climate variables functions
Var.ToChangeName <- function ( varName )
{
    # have to hardwire names here; they should match where defined elsewhere
    stdVarNames <- c( "Temp", "Tmax", "Tmin", "Precip" )
    if ( Var.StandardName(varName, verbose=FALSE) == "Precip" )
	return( "PrecipChg" )
    else if ( substr( Var.StandardName(varName, verbose=FALSE), 1, 1 ) == "T" )
	return( "TempChg" )
    else
	return( varName )	# safe return
}

#------------------------------------------------------------------------------------------------------------
#	RCP names
#------------------------------------------------------------------------------------------------------------

#' Given an RCP string, make sure it is in a form ready for use in final titles, etc.
#'
#' If a vector of strings is supplied, unrecognized strings are stripped from
#' the result.
#'
#' @param rcpStr	One or more strings containing an RCP name.
#' @return		One or more strings containing an RCP name in "proper" form, or NULL if none found.
#' @export
#' @concept	variables experiments
#' @family climate variables functions
RCP.DisplayName <- function ( rcpStr=NULL )
{
    displayNames <- c( "Historical", "Historical",
			"RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5",
			"RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5" )
    searchable   <- c( "baseline", "historical",
			"rcp26",  "rcp45",  "rcp60",  "rcp85",
			"rcp2p6", "rcp4p5", "rcp6p0", "rcp8p5" )

    if ( is.null(rcpStr) )
	return( displayNames[1:5] )

    rcpStrLC <- tolower( rcpStr )		# lower case
    rcpStrLC <- gsub( "[ .]", "", rcpStrLC )	# remove spaces and dots
    i <- grep( rcpStrLC, searchable )		# find match
    if ( length(i) > 0 )
	return( displayNames[i] )
    else
	return( NULL )
}

RCP.Description <- function( iRCP )
{
    # pair of ( W/m2, climate action )
    desc <- list( c( "2.6", "substantial" ),
	       c( "4.5", "moderate" ),
	       c( "6.0", "minimal" ),
	       c( "8.5", "no" ) )
    prefix <- "Additional"
    suffix1 <- "W/m2 added to the atmosphere system by 2100 relative to preindustrial levels, with"
    suffix2 <- "climate mitigation"
    # combine
    rcpStr <- paste( prefix, desc[[iRCP]][1], suffix1, desc[[iRCP]][2], suffix2 )
    return( rcpStr )
}

#' Extract the RCP substring from a string.
#' @param s	A string containing tolower("rcp");
#'		the rcp substring should be delineated by one of (- _ [space]).
#' @return	string if an RCP substring is found, or NULL if not found.
#' @export
#' @concept	variables experiments
#' @family climate variables functions
ExtractRCP <- function ( s )
{
    i <- regexpr( "rcp|histor", tolower(s) )
    if ( i > 0 )
    {
	ss <- substring( s, i )
	rcp <- strsplit( ss, "-|_|\ " )[[1]][1]
	if ( length(rcp) == 0 )
	    rcp <- NULL
	return( rcp )
    }
    return( NULL )
}

#' Extract known historical and RCP projection name strings from file names.
#' @param filenames	List of netCDF file names with paths; from DatasetMetadata::GetFileList
#' @return		character vector: RCP strings in "document" format, e.g., "RCP 8.5"
#' @export
#' @concept	dataset netcdf
#' @family dataset functions
# TODO: ExtractKnownRCPs: make a DatasetMetadata class member, specialized for each subclass
ExtractKnownRCPs <- function( filenames )
{
    rcps.known <- c( "historical", "rcp26",   "rcp45",   "rcp60",   "rcp85" )
    rcp.proper <- c( "historical", "RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5" )

    fileNameParts.list <- strsplit(basename(filenames), "_")
    #   [[1]]
    #   [1] "BCSD"   "0.125deg"  "pr"  "Amon"  "MPI-ESM-LR"  "rcp85" "r1i1p1" "200601-210012.nc"
    # extract RCP strings from file names
    i <- grep( "rcp", fileNameParts.list[[1]] )		# element containing rcp
    rcp <- unlist( lapply(fileNameParts.list, "[", i) )	# e.g., [1] "rcp45" "rcp85"
    rcp <- unique( sort(rcp) )				# e.g., "rcp45" "rcp85"
    rcp <- factor(rcp, levels=rcps.known, ordered=TRUE)	# e.g., [1] rcp45, rcp85, Levels: historical...
    rcp <- rcp.proper[rcp]				# e.g., [1] "RCP 4.5" "RCP 8.5"
    return( rcp )
}

IsHistorical <- function( rcp )
{
    return( startsWith( tolower(rcp), "hist" ) )
}

# end
