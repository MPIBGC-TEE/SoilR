#' deprecated function that used to create an object of class SoilR.F0
#' 
#' The function internally calls the constructor of the replacement class
#' \code{\link{ConstFc-class}}.
#' 
#' 
#' @param values a numeric vector
#' @param format a character string describing the format e.g. "Delta14C"
#' @return An object of class \code{\link{ConstFc-class}} that contains data
#' and a format description that can later be used to convert the data into
#' other formats if the conversion is implemented.
 SoilR.F0.new <- function 
     (
     values=c(0),  
     format="Delta14C"   
     )
     {
      warning(WarningConstFc())
 	F0=ConstFc(values=values,format=format)
 	return(F0)
 }
