##################################################
###   FUNCTIONS USED FOR STEPS PROCESS  ##########
##################################################

#' Remove NULL lists and dataframes
#'  
#'  @param x List or nested dataframe in which entire components are NULL             

rmNull <- function(x) {
  x <- Filter(Negate(is.null), x)
  lapply(x, function(x) if (is.list(x)) rmNull(x) else x)
}

