#------------------------------------------------------------------------------------------------------------
# file:         PlotBase.R
# project:      ClimatePrimers
# organization: Natural Resource Ecology Laboratory at Colorado State University.
#               Previously: North Central Climate Adaptation Science Center (nccsc.colostate.edu)
# description:  Base class for plots. R class type is RC.
# author:       Thomas E. Hilinski <tom.hilinski@colostate.edu>
# license:      See LICENSE.md or https://unlicense.org
#------------------------------------------------------------------------------------------------------------

#' class PlotBase
#'
#' Base class for climate primer plots.
#'
#' @field name		string: name of the plot
#' @field description	string: Description of the plot; used for plot catalog; is implmented in child class
#' @field caption	string: Caption for the plot;  is implmented in child class
#' @field regionName	string: name of region, state, or park
#' @field filename	string: file name to write plot to (optional)
#' @aliases PlotBase
# Can create and extend:
#' @export PlotBase
#' @exportClass PlotBase
#' @concept	plots climate-plots
#' @family plot and map classes
PlotBase <- setRefClass( "PlotBase",
    contains = "VIRTUAL",

    fields = c( name        = "character",
		description = "character",
		caption     = "character",
		regionName  = "character",
		filename    = "character" ),
    methods = list(

	#' @export
	initialize = function( regionName = NULL, name=class(.self)[1] )
	{
	    "\\cr
	    Plot class constructor.

	    \\strong{Arguments}
	    \\describe{
	      \\item{ regionName }{ \\cr string: name of region, state, or park }
	      }
	    "

	    .self$name        <- name		# constant
	    if ( !missing(regionName) && !IsEmpty(regionName) )
		.self$regionName <- regionName	# constant
	    .self$description <- Describe()	# default is built here; child class can override
	    .self$caption     <- NA_character_	# default is built here; child class can override
	    .self$filename    <- NA_character_	# default is built here; child class can override
	},

	#' @export
	Name = function()
	{
	    "\\cr
	    Name of plot; used in configuration file and report.

	    \\strong{Arguments}
	    \\describe{
	      \\item{ return }{ \\cr string: name of plot }
	      }
	    "

	    return( name )
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

	    # default description has the form:
	    #   "plot-name shows a plot for regionName"
	    if ( IsEmpty( description ) )
	    {
		if ( !IsEmpty(regionName) )
		    description <<- paste( name, "shows a plot for", regionName )
		else
		    description <<- paste( name, "plot." )
		# end with a period.
		if ( description[ length(description) ] != "." )
		    description <<- paste( description, ".", sep="" )
	    }
	    return( description )
	},

	#' @export
	Caption = function( ... )
	{
	    "\\cr
	    Produce a caption for the WLDiagram suitable for the climate primer.

	    \\strong{Arguments}
	    \\describe{
	      \\item{ return }{ \\cr string: plot caption }
	      }
	    "

	    # default caption has the form:
	    #   "plot-name: regionName ...
	    if ( IsEmpty( caption ) )
	    {
		caption <<- name
		parts <- c( regionName, ... )
		if ( length(parts) > 0 )
		{
		    caption <<- paste( caption, "for", parts, collapse=" " )
		}
		# end with a period.
		if ( caption[ length(caption) ] != "." )
		    caption <<- paste( caption, ".", sep="" )
	    }
	    return( caption )
	},

	#' @export
	FileName = function( extension="png", path=NULL, ... )
	{
	    "\\cr
	    Make a plot file name, e.g., ElevationMap.png

	    \\strong{Arguments}
	    \\describe{
	      \\item{ extension }{ \\cr string: file name extension for type of file; default is png. }
	      \\item{ path }{ \\cr string: plot file path, or NULL if no path }
	      \\item{ return }{ \\cr plot file name }
	      }
	    "

	    if ( IsEmpty( filename ) )
	    {
		fn <- NULL
		parts <- c( name, regionName, ... )
		if ( !IsEmpty(regionName) && length(parts) < 2 )
		    warning( "File name parts were not specified." )
		fn <- paste( parts, collapse="_" )
		fn <- gsub( " ", "_", fn )
		if ( !IsEmpty(path) )
		    fn <- file.path( path, fn )
		if ( !IsEmpty(extension) )
		    fn <- paste( fn, extension, sep="." )
		filename <<- fn
	    }
	    return( filename )
	},

	#' @export
	MakeConfig = function( ini.plots, ini.colors, ... )
	{
	    "\\cr
	    Make a list of graphical configuration elements.

	    \\strong{Arguments}
	    \\describe{
	      \\item{ ini.plots }{ \\cr list: section [plots] or [maps] from the configuration file }
	      \\item{ ini.colors }{ \\cr list: section [colors] from the configuration file }
	      \\item{ ... }{ \\cr list: optional; additional named plot configuration values }
	      \\item{ return }{ \\cr list of plot configuration values }
	      }
	    "

	    # TODO: modify title and labels as appropriate
	    title <- paste( regionName, " for ", name, sep="" )
	    xlab  <- "X"
	    ylab  <- "Y"

	    # make a config .Object with default elements
	    config <- MakePlotConfig( filename,
					title=title, xlab=xlab, ylab=ylab,
					width=ini.plots$width, height=ini.plots$height,
					colors=GetPalette() )
	    # names( config )
	    # [1] "fileName" "title"  "xlab"  "ylab"  "cex"  "width"  "height"  "doLegend" "colors"

	    args <- list(...)
	    if ( length(args) > 0 )
		config <- append( config, args )

	    return( config )
	},

	#' @export
	Plot = function( graphConfig, ... )
	{
	    "\\cr
	    Make a plot, returning a grob.

	    \\strong{Arguments}
	    \\describe{
	      \\item{ graphConfig }{ \\cr list: plot configuration values }
	      \\item{ return }{ \\cr A plot object; a 'grob' produced by package 'grid' }
	      }
	    "

	    warning( "Method \"Plot\" is not implemented for this plot class." )
	    return( NULL )
	},

	#' @export
	summary = function ( className = class(.self)[1] )
	{
	    "\\cr
	    Print a summary description of the object.
	    "

	    msg <- paste( "Object of class ", className,
			  "\nplot name:   ", Name(),
			  "\nregion name: ", regionName,
			  "\nfile name:   ", FileName(),
			  "\ndescription:\n",
			  sep="" )
	    msg2 <- paste( strwrap( Describe(), width=70, indent=2, exdent=2 ),
			   collapse='\n' )
	    msg <- paste( msg, msg2, sep="" )
	    writeLines( msg )
	}
    ) )

suppressMessages(
    setMethod( "summary", signature(object = "PlotBase" ),
	function( object, ... )
	{
	    object$summary()
	} ) )

setMethod( "show", signature(object = "PlotBase"),
    function( object )
    {
	object$summary()
    } )

# end
