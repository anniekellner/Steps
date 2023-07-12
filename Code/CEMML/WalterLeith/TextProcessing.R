#------------------------------------------------------------------------------------------------------------
# file:         TextProcessing.R
# project:      ClimatePrimers
# organization: Natural Resource Ecology Laboratory at Colorado State University.
#               Previously: North Central Climate Adaptation Science Center (nccsc.colostate.edu)
# description:  Functions for text processing tasks.
# author:       Thomas E. Hilinski <tom.hilinski@colostate.edu>
# license:      See LICENSE.md or https://unlicense.org
#------------------------------------------------------------------------------------------------------------

#' Capitalize all words in a string.
#' @param s	String of space-delimited words.
#' @return	String with words capitalized.
#' @export
#' @concept strings utilities
#' @family text functions
CapWords <- function( s )
{
    if ( is.character(s) )
	return( gsub("(\\b[a-z])", "\\U\\1", tolower(s), perl=TRUE) )
    else
	return( s )
}

#' Make a sentence fragment from a vector of strings.
#' @param sv	vector of strings
#' @return	string
#' @examples \dontrun{
#' StringsToSentencePart( LETTERS[1:3] )
#' # returns: "A, B and C"
#' }
#' @export
#' @concept strings utilities years
#' @family text functions
StringsToSentencePart <- function( sv )  # sv = string vector
{
    if ( length(sv) > 1 )
    {
    	txt <- paste( sv[1:(length(sv) - 1) ], collapse=", " )
    	txt <- paste( txt, "and", sv[length(sv)] )
    	return( txt )
    }
    else
    	return( sv )
}

#' Make a string from a pair of values, or a list of pairs.
#'
#' @param pairs		integer vector or list of vectors: a pair of values, or list of value pairs
#' @param sep		character: a separator between the values; the default is '-'
#' @return		A string, or a vector of strings.
#' @examples \dontrun{
#'   # provide a pair of years to get a range as a string:
#'   PairsToString( c(2026, 2035) )
#'   # returns '2026-2035'
#'   PairsToString( list( c(2026, 2035), c(2046, 2055) ) )
#'   # returns '2026-2035, 2046-2055'
#' }
#' @export
#' @concept strings utilities years
#' @family text functions
PairsToString <- function( pairs, sep="-" )
{
    str <- NULL
    if ( is.list(pairs) )
    {
	for ( tp in pairs )
	    str <- c( str, paste( tp, collapse=sep ) )	# [1] "2026-2035" "2046-2055"
	str <- toString(str)				# [1] "2026-2035, 2046-2055"
    }
    else # vector of 2 years
	str <- c( str, paste( pairs, collapse=sep ) )	# [1] "2026-2035"
    return( str )
}

#' Construct a sentence
#'
#' Construct a sentence from parts, and append a period to the end of a string.
#' Arguments that are numeric and vectors are converted to a character string.
#'
#' @param ...		Parts to the sentence, in order of construction.
#' @param forceCap	logical: if TRUE capitalize the first character;
#'			if FALSE, to not alter the first character;
#'			default is TRUE.
#' @param endPunc	Punctuation to put at end of the sentence. Default is a period.
#' @return		A string, or the argument if it is not a string.
#' @examples \dontrun{
#' Sentence("hi", "there", letters[1:3], 1:3)
#' # [1] "Hi there a b c 1 2 3."
#' }
#' @export
#' @concept strings utilities
#' @family text functions
Sentence <- function ( ..., forceCap=TRUE, endPunc="." )
{
    parts <- list(...)
    if ( length(parts) == 0 )
	return( "" )
    # convert vectors into a string
    for ( i in 1:length(parts) )
    {
	if ( length( parts[[i]] ) > 1 )
	    parts[[i]] <- paste( parts[[i]] , collapse=" " )
    }
    # make a sentence
    s <- paste( parts, collapse=" " )
    if ( nchar(s) > 0 )
    {
	if ( substring( s, nchar(s) ) != endPunc )
	    s <- paste( s, endPunc, sep="" )
	if ( forceCap  )
	    substr(s, 1, 1) <- toupper( substr(s, 1, 1) )
    }
    return( s )
}

#' Format a paragraph
#'
#' Make a single string into a wrapped paragraph ending in a period.
#'
#' @param str		A string
#' @param wrap		maximum string length after wrap; the 'column' at which to wrap.
#' @param sep		Line separator; the default is newline.
#' @param prefix	Specifies text or other characters to be written
#'			before the first line. E.e., spaces for indentation.
#'			Default is NULL (no prefix).
#' @param sentence	logical: if TRUE, paragraph ends in a sentence, and a period is appended if needed;
#'			if FALSE, no period is appended; default is TRUE.
#' @return		A vector of strings, each ending in sep
#' @examples \dontrun{
#'   str <- "Make a string into a wrapped paragraph ending in a period"
#'   writeLines( Para( str, 30 ) )
#' }
#' @export
#' @concept strings utilities
#' @family text functions
Para <- function( str, wrap=65, sep="\n", prefix=NULL, sentence=TRUE )
{
    if ( is.numeric(wrap) )
    {
	if ( is.null(prefix) )
	    prefix <- ""
	str <- strwrap( str, wrap, initial=prefix )
	str <- paste( str, collapse=sep )
    }
    # end with a period.
    if ( sentence )
	str <- Sentence( str )
    return( str )
}

#' Make a pretty string from longitude and latitude.
#' @param lon		numeric: longitude
#' @param lat		numeric: latitude
#' @param digits	integer: number of decimal places for seconds; default is 3
#' @return		string: longitude and latitude in 'degrees minutes seconds' form.
#' @concept strings utilities
#' @family text functions
Pretty.LonLat <- function( lon, lat, digits=3 )
{
    Parts <- function( value, digits )
    {
	degrees <- abs( as.integer( trunc(value) ) )
	min.flt <- ( abs(value) - degrees ) * 60.0
	minutes <- as.integer( trunc( min.flt ) )
	seconds <- round( (min.flt - minutes) * 60.0, digits=digits )
	return( c(degrees, minutes, seconds) )
    }

    ToString <- function( parts, ltr )
    {
	return( paste( paste( parts[1], ltr, sep="" ),
		       paste( parts[2], "\'", sep="" ),
		       paste( parts[3], "\"", sep="" ) ) )
    }

    lon.parts <- Parts( lon, digits )
    lon.letter <- if (lon < 0.0) "W" else "E"
    lon.str <- ToString( lon.parts, lon.letter )

    lat.parts <- Parts( lat, digits )
    lat.letter <- if (lat < 0.0) "S" else "N"
    lat.str <- ToString( lat.parts, lat.letter )

    return( paste( lon.str, lat.str, sep=", " ) )
}

#' Split a string containing a key-value pair into 2 values.
#'
#' @param s	string: contains a key-value pair, e.g., "file=Important.data"
#' @param sep	character: separator between the key and value; default is '='.
#'		The key is the first value followed by a separator.
#' @return	vector containing 2 string, or NULL if could not split the input string.
#' @export
#' @concept strings utilities
#' @family text functions
#' @family utility functions
SplitKV <- function( s, sep="=" )
{
    if ( !is.character(s) )
    	return( NULL )
    kv <- unlist( strsplit( s, sep ) )
    if ( length(kv) < 2 )
    	return( NULL )
    if ( length(kv) > 2 )
    {
	kv[2] <- paste( kv[2:length(kv)], collapse=sep )
	kv <- kv[1:2]
    }
    kv <- sapply( kv, base::trimws )
    names(kv) <- c("key", "value")
    return(kv)
}

#' Add prefix and suffix characters to a string.
#' @param s		string to enclose in before and after characters
#' @param before	character: 's' will be concatenated to this; default is double-quote
#' @param after		character: this will be concatenated to 's'; default is 'before'
#' @return		string enclosed in 'before' and 'after'
Enclose <- function( s, before="\"", after=before )
{
    return( paste( before, s, after, sep="" ) )
}


# end
