#' Atmospheric 14C fraction
#' 
#' @docType data
#'
#' @usage data(C14Atm)
#'
#' @format
#' 	A data frame with 108 observations on the following 2 variables.
#' 	\enumerate{ 
#'    \item V1 a numeric vector 
#"    \item V2 a numeric vector 
#' }
#' @description
#' 	Atmospheric 14C fraction in units of Delta14C for the bomb period in the
#' 	northern hemisphere.
#' 	
#' 	@note
#' 	This dataset will be deprecated soon. Please use \link{C14Atm_NH} or
#' 	\link{Hua2013} instead.
#' 	
#' 
#' @examples
#' #Notice that C14Atm is a shorter version of C14Atm_NH
#' require("SoilR")
#' data("C14Atm_NH")
#' plot(C14Atm_NH,type="l")
#' lines(C14Atm,col=2)
#' 
#' @keywords datasets
'C14Atm'
