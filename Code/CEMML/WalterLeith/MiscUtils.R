#------------------------------------------------------------------------------------------------------------
# file:         MiscUtils.R
# project:      ClimatePrimers
# organization: Natural Resource Ecology Laboratory at Colorado State University.
#               Previously: North Central Climate Adaptation Science Center (nccsc.colostate.edu)
# description:  Contains functions from IRC library (Misc.R) plus additional functions.
# author:       Thomas E. Hilinski <tom.hilinski@colostate.edu>
# license:      See LICENSE.md or https://unlicense.org
#------------------------------------------------------------------------------------------------------------

#' Get the R version number as either a string (default), or a pair of numbers (major, minor version).
#' @param as.string	If TRUE, return as string, else return as a vector.
#' @return		A string or a pair of numbers (major, minor version).
#' @export
#' @concept utilities
#' @family utility functions
RVersion <- function (as.string = TRUE)
{
    if ( as.string )
    {
	return( paste( R.version$major, R.version$minor, sep="." ) )
    }
    else # as vector pair of numbers (major, minor)
    {
	return( c( as.integer( R.version$major ), as.numeric( R.version$minor ) ) )
    }
}

#' Display a range of positions in an object.
#'
#' For a data.frame, offset is the number of rows to display before and after the position.
#' For a vector, offset is the number of elements to display before and after the position.
#'
#' @param obj		Object to display.
#' @param pos		Position (row or index) to center the display at.
#' @param offset	Row or index distance from position, before and after, to display.
#' @export
#' @concept utilities numeric
#' @family utility functions
Around <- function ( obj, pos, offset = 1 )
{
    if ( is.data.frame(obj) )
    {
	if ( nrow(obj) > 0 )
	{
	    pos.before <- max( 1, pos - offset )
	    pos.after  <- min( nrow(obj), pos + offset )
	    i <- pos.before:pos.after
	    df <- obj[ pos.before:pos.after, ]
	    # TODO: do not change the names; rather prepend an row number column
	    # row.names( df ) <- as.character( i )
	    print( df )
	}
    }
    else if ( is.vector(obj) )
    {
	if ( length(obj) > 0 )
	{
	    pos.before <- max( 1, pos - offset )
	    pos.after  <- min( nrow(obj), pos + offset )
	    i <- pos.before:pos.after
	    v <- obj[ i ]
	    names( v ) <- as.character( i )
	    print( v )
	}
    }
#    else if ( class( obj ) == "SpatialGridDataFrame" )
#    {
#	# TODO: if ( class( obj ) == "SpatialGridDataFrame" )
#    }
    else
    {
	stop( "Around( ", class(obj), " ) is not implemented" )
    }
}

#' Examines object to see if it is not NULL or NA.
#' @param x	Object to examine.
#' @return	TRUE if x is a valid object, else FALSE.
#' @export
#' @family utility functions
IsValid <- function (x)
{
    bool <- is.null(x)
    if ( !bool )
    {
	if ( isS4(x) )
	    return( TRUE )
	isSpatial <- length( grep( "^Spatial", class(x) ) ) > 0
	if ( isSpatial )
	    return( length(x) == 0 )
	if ( length(x) > 1 )
	    bool <- all( is.na(x) )		# can return NULL for some objects
	else
	    bool <- is.na(x)		# can return NULL for some objects
    }
    if ( is.null(bool) )
	bool <- FALSE
    return ( !bool )
}

#' Examines object to see if it contains data.
#' @param x	Object to examine.
#' @return	TRUE if x contains data, else FALSE.
#' @export
#' @family utility functions
IsEmpty <- function (x)
{
    if ( length(x) == 0 )
	return( TRUE )
    if ( is.null(x) )
	return( TRUE )
    if ( is.data.frame(x) )
	return( nrow(x) == 0 )
    if ( is.list(x) )
	return( length(x) == 0 )
    isSpatial <- length( grep( "^Spatial", class(x) ) ) > 0
    if ( isSpatial )
	return( length(x) == 0 )
    if ( IsValid(x) )
    {
	if ( isS4(x) )
	    return( FALSE )
	if ( is.character(x[1]) )
	    return ( nchar(x[1], "bytes") == 0 )
	else
	    return ( length(x) == 0 )
    }
    return( TRUE )
}

#' Counts the TRUE elements of a.
#' @param a	Array of logical values.
#' @return	Integer >= 0.
#' @export
#' @family utility functions
CountWhich <- function (a)
{
    return( length( which( a ) ) )
}

#' Counts the occurrences of a in b.
#' @param a	Single object to find.
#' @param b	Array or vector of objects to search in.
#' @return	Integer >= 0.
#' @export
#' @family utility functions
Count.AinB <- function (a,b)
{
    return( length( which( a %in% b ) ) )
}

#' Count then number of NA values.
#' @param a	Single vector to examine.
#' @return	Integer >= 0.
#' @export
#' @family utility functions
Count.NA <- function (a)
{
    return( length( which( is.na(a) ) ) )
}

#' Count then number of non-NA values.
#' @param a	Single vector to examine.
#' @return	Integer >= 0.
#' @export
#' @family utility functions
Count.NotNA <- function (a)
{
    return( length( which( !is.na(a) ) ) )
}

#' Does an S4 object have a slot?
#' @param	any object
#' @return	logical: TRUE if obj is an S4 class and has the slot nameSlot, else FALSE.
#' @export
#' @concept utilities
#' @family utility functions
HasSlot <- function( obj, nameSlot )
{
    result <- FALSE
    if ( isS4(obj) )
	result <- nameSlot %in% slotNames(obj)
    return( result )
}

#' Does a reference class object have a field?
#' @param	any object
#' @return	logical: TRUE if obj is a reference class and has the field nameField, else FALSE.
#' @export
#' @concept utilities
#' @family utility functions
HasField <- function( obj, nameField )
{
    result <- FALSE
    if ( is( obj, "refClass" ) )
	result <- nameField %in% names( obj$getClass()@fieldClasses )
    return( result )
}

#------------------------------------------------------------------------------------------------------------
#	conversions
#------------------------------------------------------------------------------------------------------------

#' Converts elements == value to NA.
#' @param v		Object to search for value in.
#' @param findValue	Value to search for.
#' @return		Modified copy of original object.
#' @export
#' @family utility functions
ValueToNA <- function ( v, findValue )
{
    if ( class(v)[1] == "SpatialGridDataFrame" )
	v[[1]][ which( v[[1]] %in% findValue ) ] <- NA
    else
	v[ which( v %in% findValue ) ] <- NA
    return (v)
}

#' Converts elements == NA to value.
#' @param v		Object to search for value in.
#' @param newValue	Value to replace NA elements.
#' @return		Modified copy of original object.
#' @export
#' @family utility functions
NAtoValue <- function ( v, newValue )
{
    if ( class(v)[1] == "SpatialGridDataFrame" )
	v[[1]][ which( is.na( v[[1]] ) ) ] <- newValue
    else
	v[ which( is.na(v) ) ] <- newValue
    return (v)
}

#' Convert a logical or boolean value to a "yes|no" string.
#' @param  value	logical: a boolean value or result of an expression
#' @return string
#' @export
#' @family utility functions
BooleanToString <- function( value )
{
    return( ifelse( value, "yes", "no" ) )
}

#' Convert a "yes|no", "true|false", or "0|1" field to a boolean value, or NULL if not determined.
#' @param  value		string to be converted to a boolean value.
#' @param  unknownIsFalse	If TRUE, an unrecognized value is interpreted as FALSE; default is FALSE.
#' @return boolean
#' @export
#' @family utility functions
ToBoolean <- function ( value, unknownIsFalse=FALSE )
{
    trueValues  <- c( "yes", "y", "true",  "t", "1" )
    falseValues <- c( "no",  "n", "false", "f", "0" )
    value <- tolower(value)
    if ( value %in% trueValues )
	return( TRUE )
    else if ( value %in% falseValues )
	return( FALSE )
    else
    {
	if ( unknownIsFalse )
	    return( FALSE )
	else
	    return(NULL)
    }
}

#------------------------------------------------------------------------------------------------------------
#	year ranges
#------------------------------------------------------------------------------------------------------------

#' Does the argument contain year ranges?
#' @param yearRanges	a list of year pairs, or a single year pair
#' @param doWarning	logical: if TRUE, display a warning if the yearRanges is not recognized;
#'			default is FALSE
#' @return	TRUE if seems to contain year ranges, or FALSE if not
#' @export
#' @concept utilities time years
#' @family utility functions
IsYearRanges <- function (yearRanges, doWarning=FALSE)
{
    ok <- !IsEmpty( yearRanges )
    if ( ok && is.list(yearRanges) )
    {
	ok <- ( is.vector( yearRanges[[1]] ) && length( yearRanges[[1]] ) == 2 )
	if ( ok )
	    ok <- is.numeric( yearRanges[[1]][1] )
    }
    else if ( ok && is.vector(yearRanges) )
    {
	ok <- ( is.vector( yearRanges ) && length( yearRanges ) == 2 )
	if ( ok )
	    ok <- is.numeric( yearRanges[1] )
    }
    if ( !ok && doWarning )
	warning( "Found one or more invalid year ranges." )
    return( ok )
}

#' Is a value within a range?
#' @param x		Numeric: value to compare to the range
#' @param xRange	Numeric vector of length 2: values bounding the range
#' @param inclusive	logical: if TRUE (default), include the bounding values of the range;
#'			if FALSE, exclude the bounding values.
#' @return		logical
#' @export
#' @concept utilities time years
#' @family utility functions
InRange <- function( x, xRange, inclusive=TRUE )
{
    if ( inclusive )
	return( x >= xRange[1] & x <= xRange[2] )
    else
	return( x > xRange[1] & x < xRange[2] )
}

#' Is the first range inclusively contained within the second range?
#' @param r1	Numeric vector of length 2: first range
#' @param r2	Numeric vector of length 2: second range
#' @return		logical
#' @export
#' @concept utilities time years
#' @family utility functions
RangeInRange <- function( r1, r2 )
{
    if ( length(r1) < 2 || length(r2) < 2 )
    {
	warning( "Both range vectors must have at least 2 values." )
	return( FALSE )
    }
    return( r1[1] >= r2[1] & r1[2] <= r2[2] )
}

#' Extract 4-digit years from one or a vector of strings (e.g., file names).
#'
#' Assumes that each string contains only one 4-digit number.
#'
#' @param stringsWithYears	One or more strings containing 4-digit numbers.
#' @param sortValues		If TRUE sort the resulting vector, else keep in same order as input strings;
#'				the default is to sort.
#' @return A vector of integers, or NULL if none were found.
#' @export
#' @concept strings utilities time years
#' @family text functions
#' @family utility functions
ExtractYearsFromStrings <- function ( stringsWithYears, sortValues=TRUE )
{
    yearStrs <- regmatches( stringsWithYears, gregexpr( "[0-9]{4}", stringsWithYears ) )[[1]]
    if ( length(yearStrs) > 0 )
    {
	years <- as.integer( yearStrs )
	if ( sortValues )
	    years <- sort(years)
	return( years )
    }
    return( NULL )
}

#' Extract ranges of 4-digit years from one or a vector of strings (e.g., file names).
#'
#' Assumes that each string contains only one range of 4-digit number in the form "YYYY-YYYY".
#'
#' @param strs		One or more strings containing 4-digit numbers.
#' @param as.years	If TRUE convert the range strings to a list of integer pairs;
#'			if FALSE, return ranges as a vector of strings;
#'			the default is TRUE.
#' @return	A vector or list of integer pairs in the same order as 'strs', or NULL if none were found.
#' @examples \dontrun{
#'   ExtractYearRangesFromStrings( c('2026-2035', '2046-2055') )
#'   # returns
#'   #   [[1]]
#'   #   [1] 2026 2035
#'   #   [[2]]
#'   #   [1] 2046 2055
#'   ExtractYearRangesFromStrings("Precip_rcp45_2026_2035_VTS_daily.RData")
#'   # returns
#'   #   [1] 2026 2035
#' }
#' @export
#' @concept strings utilities time years
#' @family text functions
#' @family utility functions
ExtractYearRangesFromStrings <- function ( strs, as.years=TRUE )
{
    if ( IsEmpty(strs) )
	return( NULL )

    yrStrs <- sapply( strs,
		      FUN=function(s)
		      {
			i <- regexpr( "[0-9]{4}(-|_)[0-9]{4}", s )
			if ( i > 0 )
			    return( substr(s, i, i+8 ) )
			else
			    return( NULL )
		      } )
    if ( all( sapply( yrStrs, is.null ) ) )
	return( NULL )
    # e.g., yrStrs == c( "2026-2035", "2046-2055" )
    if ( as.years )	# convert to list of year pairs
    {
	# yearPairs <- strsplit( yrStrs, "-|_" )	# now a list of string pairs
	yearPairs <- sapply( yrStrs,
			     FUN=function(s)
			     {
				if ( is.null(s) ) return(NULL)
				else              lapply( strsplit( s, "-|_" ), as.integer )
			     } )
	names(yearPairs) <- NULL
	if ( length(yearPairs) == 1 )
	    yearPairs <- unlist( yearPairs )
	return( yearPairs )			# now a list of integer pairs
    }
    else
	return( yrStrs )
}

#' Get strings which contain the year range.
#'
#' Year range in the strings must have the form 'XXXX-YYYY'.
#'
#' @param strs		One or more strings containing pairs of 4-digit years of the form 'XXXX-YYYY'.
#' @param yearRange	One integer year or a pair of years in ascending order.
#' @return		Character vector, or NULL if search failed.
#' @export
#' @concept strings utilities time years
#' @family text functions
#' @family utility functions
StringsWithYearRange <- function ( strs, yearRange )
{
    # check args
    stopifnot( length(strs) > 0 )
    if ( length(yearRange) == 1 )
	yearRange[2] <- yearRange
    stopifnot( IsYearRanges(yearRange) )

    # get year ranges from file names, in same order as file names
    yearRanges.all <- ExtractYearRangesFromStrings( strs )	# returns list of int pairs
    if ( IsEmpty( yearRanges.all[1] ) )	# none found?
	return( NULL )
    # find intervals containing yearRange
    iYRs <- NULL
    if ( is.list(yearRanges.all) )
    {
	for ( i in 1:length( yearRanges.all ) )
	{
	    if ( yearRange[1] >= yearRanges.all[[i]][1] && yearRange[2] <= yearRanges.all[[i]][2] )
		iYRs <- c( iYRs, i )
	}
    }
    else # one range only
    {
	if ( yearRange[1] >= yearRanges.all[1] && yearRange[2] <= yearRanges.all[2] )
	    iYRs <- 1
    }
    if ( length(iYRs) > 0 )
	return( strs[iYRs] )
    else
	return( NULL )
}

#------------------------------------------------------------------------------------------------------------
#	text functions
#------------------------------------------------------------------------------------------------------------

#' Filter a vector of strings by another vector of strings.
#' @param toFind	vector of strings to search for
#' @param strings	vector of strings to search within
#' @param invert	logical: if TRUE, return the strings which were not found; default is FALSE.
#' @return		subset of argument strings, or NULL if no matches were found.
#' @export
#' @concept strings utilities
#' @family text functions
#' @family utility functions
FilterStrings <- function( toFind, strings, invert=FALSE )
{
    i <- unlist( sapply( toFind, function(x, v) which( grepl(x, v) ), strings ) )
    # i =  vector of indices to arg strings
    if ( length(i) > 0 )
    {
	if ( invert )
	    i <- which( ! 1L:length(strings) %in% i )
	return( strings[i] )
    }
    else
	return( NULL )
}

# Sort a vector of strings by substrings, then for each group sort by values.
# @param strVec	character vector: strings to search for tags using template and values.
# @param template	string: Pattern template containing one instance of the string 'XXX' which will
#			be replaced by each tag.
# @param tags		character vector: strings to search for using template.
# @param values	Integer vector of the same length as strVec containing unique sorted values.
# @return	A vector of sorted strings
# @export
# @concept strings utilities
# @family text functions
# @family utility functions
SortBySubstrAndInteger <- function( strVec, template, tags, values )
{
    iSort <- order(values)	# sort order indices
    vecSorted <- NULL
    for ( tag in tags )
    {
	pattern <- sub( "XXX", tag, template )			# no checks for success
	strVec.match <- strVec[ grep( pattern, strVec ) ]	# find matches
	stopifnot( length(values) == length(strVec.match) )
	vecSorted <- c( vecSorted, strVec.match[ iSort ] )
    }
    return( vecSorted )
}

DF.Match.NRows <- function( df1, df2, fill=NA )
{
    AddRows <- function( df, numToAdd, fillRow )
    {
	for ( i in 1:numToAdd )
	    df <- rbind( df, fillRow )
	return( df )
    }

    if ( nrow(df1) > nrow(df2) )
    {
	fillRow <- rep( fill, ncol(df2) )
	df2 <- AddRows( df2, nrow(df1) - nrow(df2), fillRow )
    }
    else if ( nrow(df2) > nrow(df1) )
    {
	fillRow <- rep( fill, ncol(df1) )
	df1 <- AddRows( df1, nrow(df2) - nrow(df1), fillRow )
    }
    return( list(df1, df2) )
}

#------------------------------------------------------------------------------------------------------------
#	User interaction
#------------------------------------------------------------------------------------------------------------

#' Make a time stamp for messages.
#' @param date		logical: if TRUE, include the date in the time stamp; default is FALSE.
#' @param brackets	logical: if TRUE, enclose the time stamp in brackets; default is TRUE.
#' @param prefix	string: if not NULL, the string will be included at the start of the timestamp;
#'			the default is NULL.
#' @export
#' @concept utilities
#' @family utility functions
Timestamp <- function( date = FALSE, brackets = TRUE, prefix=NULL )
{
    if ( date )
	timestamp <- format( Sys.time(), "%Y-%m-%d %T" )
    else
	timestamp <- format( Sys.time(), "%T" )
    if ( !is.null(prefix) )
	timestamp <- paste( prefix, timestamp )
    if ( brackets )
	return( paste( "[", timestamp, "]", sep="" ) )
    else
	return( timestamp )
}

#' Display a message to stdout with a timestamp prefix.
#' @param msg		string containing message
#' @param timeStamp	logical: if TRUE, prefix message with a time stamp; default is TRUE.
#' @param prefix	NULL or string: if not NULL, the string is displayed before the time stamp.
#' @export
#' @concept utilities
#' @family utility functions
MsgAtTime <- function( msg, timeStamp=TRUE, prefix=NULL )
{
    if ( is.null(prefix) )
    {
	if ( timeStamp )
	    message( Timestamp(), " ", msg )
	else
	    message( msg )
    }
    else
    {
	if ( timeStamp )
	    message( Timestamp(), " ", prefix, msg )
	else
	    message( prefix, msg )
    }
}

#' Pause user interaction to wait for keypress.
#' @param nlBefore	If TRUE, print a newline before prompt; default is FALSE.
#' @param nlAfter	If TRUE, print a newline after keypress; default is FALSE.
#' @export
#' @concept utilities control
#' @family utility functions
# Thanks to https://diego.assencio.com/?index=86c137b502561d44b8be02f06d80ee16
PauseForKeypress <- function ( nlBefore=FALSE, nlAfter=FALSE )
{
    prompt <- "Press Enter to continue:"
    if ( nlBefore )
	prompt <- paste( "\n", prompt, sep="" )
    if ( interactive() )
    {
	return( invisible( readline( prompt=prompt ) ) )
    }
    else # display prompt; wait for keypress
    {
	cat( prompt )
	return( invisible( readLines( file("stdin"), 1 ) ) )
    }
    if ( nlAfter )
	cat( "\n" )
}

#------------------------------------------------------------------------------------------------------------
#	Error and Warning handling
#------------------------------------------------------------------------------------------------------------

#' Stop execution of a script from any function, optionally without messages.
#'
#' Modified from original source:
#' https://stackoverflow.com/questions/14469522/stop-an-r-program-without-error
#'
#' @param msg	optional message string; if provided, will be displayed; default is NULL.
#' @export
#' @concept utilities control
#' @family utility functions
StopNow <- function( msg = NULL)
{
    if ( !is.null(msg) )
	message( msg )
    opt <- options(show.error.messages = FALSE)
    on.exit( options(opt) )
    stop( msg, call.=FALSE )
}

#' Is an object of type 'try-error'?
#'
#' The "try" function, upon error, returns an object of class 'try-error'.
#' This function returns T/F checking that try() return value.
#'
#' @param value	expression that returns a value
#' @export
#' @concept utilities errors
#' @family error management functions and classes
#' @family utility functions
IsErrorClass <- function( value ) { return( inherits( value, "try-error" ) ) }

# Extract the error message part from the "try" error message.
#   try( stop("oops") )
# returns
#   "Error in try(stop(\"oops\")) : oops\n"
# This extracts:
#   "oops"
ExtractErrorMsg <- function( errMsg )
{
    return( sub( "\\n$", "",
		 substring( errMsg[1], regexpr("( : )", errMsg[1]) + 3 ) ) )
}

#' Assert with a warning.
#'
#' Using the code from the R base function stopifnot, post a warning message
#' if the expression evaluates to FALSE.
#' Unlike stopifnot, execution continues.
#'
#' @export
#' @concept utilities errors
#' @family error management functions and classes
#' @family utility functions
WarnIfNot <- function ( ... )
{
    n <- length(ll <- list(...))
    if (n == 0L)
        return( invisible() )
    mc <- match.call()
    for (i in 1L:n) if (!(is.logical(r <- ll[[i]]) && !anyNA(r) &&
        all(r))) {
        ch <- deparse(mc[[i + 1]], width.cutoff = 60L)
        if (length(ch) > 1L)
            ch <- paste(ch[1L], "....")
        warning(sprintf(ngettext(length(r), "%s is not TRUE", "%s are not all TRUE"),
            ch), call. = FALSE, domain = NA)
    }
    return( invisible() )
}

#------------------------------------------------------------------------------------------------------------
#	concurrent processing
#------------------------------------------------------------------------------------------------------------

# number of iterations needed given the number of tasks and number of cores
Parallel.NumIter <- function( numTasks, numCores = parallel::detectCores() )
{
    stopifnot( numTasks > 0 )
    stopifnot( numCores > 0 )
    numCores <- min( numCores, 120 )	# close to the usual R maximum number of connections
    n <- as.integer( trunc( numTasks / numCores ) )
    if ( n * numCores < numTasks )
	n <- n + 1L
    return(n)
}

# Make a socket cluster for concurrent processing.
# @param numTasks	Number of tasks to run; may be the number of cores available.
# @param exportPkg	logical: If TRUE, export the ClimatePrimers package to the cluster nodes.;
#			default is FALSE.
# @return		A cluster object.
MakeCluster <- function( numTasks, exportPkg=FALSE )
{
    cl <- parallel::makePSOCKcluster( numTasks )
    if ( is.null(cl) )
	stop( "Unable to create a socket cluster." )
    if ( exportPkg )
	parallel::clusterEvalQ( cl, suppressMessages( library("ClimatePrimers") ) )
    return( cl )
}

# Run the tasks on the cluster.
# @param cl	Cluster object returned by MakeCluster.
# @param X	Vector or list to distribute among the tasks.
# @param FUN	Function to run in each task.
# @param ...	Optional additional arguments, in order, to FUN.
# @return	Result of parLapply; a list, or an error object.
RunClusterTasks <- function( cl, taskName, X, FUN, ... )
{
    results.list <- try( parallel::parLapply( cl, X, FUN, ... ), silent=TRUE )
    parallel::stopCluster( cl )
    if ( IsErrorClass(results.list) )
    {
	msg <- paste( paste( taskName, ":", sep="" ),
		      "Error running concurrent tasks:",
		      ExtractErrorMsg( results.list ),
		      sep="\n" )
	stop( msg )
    }
    return( results.list )
}

#------------------------------------------------------------------------------------------------------------
#	dev and debug functions
#------------------------------------------------------------------------------------------------------------

#' Returns true if package name is in the search path, else false.
#' @param pkgNameStr	Name of R package.
#' @return		Returns boolean.
#' @export
#' @concept utilities
#' @family utility functions
IsPackageLoaded <- function (pkgNameStr)
{
    return ( 0 !=
	length(
	    grep(
		paste( "^package:", pkgNameStr, "$", sep="" ),
		search() ) ) )
}

#' Display the method names defined for the class.
#'
#' @param className	string: name of class
#' @param returnOnly	If true, return the vector of names; default is FALSE = print the names.
#' @export
#' @concept utilities
#' @family utility functions
ShowMethods <- function ( className, returnOnly=FALSE )
{
    if ( !is.character(className) )
    {
	warning( "Argument is not a character string." )
    }
    else
    {
#	x <- showMethods(classes=className, printTo=FALSE)
#	x <- unlist( lapply( strsplit( x[grep("Function: ", x,)], " "), function(x) x[2] ) )
	x <- attr(methods(class="Boundary"), "info")$generic
	if ( returnOnly )
	    return(x)
	else
	    message( toString(x) )
    }
}

# ----------------------------------------------------------------------------
