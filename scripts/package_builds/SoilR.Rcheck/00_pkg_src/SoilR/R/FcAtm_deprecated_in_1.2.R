#' FcAtm.from.Dataframe
#' 
#' This function is deprecated constructor of the deprecated class FcAtm
#' 
#' 
#' @param dframe A data frame containing exactly two columns: the first one is
#' interpreted as time the second one is interpreted as atmospheric C14 fraction
#' in the format mentioned
#' @param lag a scalar describing the time lag. Positive Values shift the
#' argument of the interpolation function forward in time. (retard its effect)
#' @param interpolation A function that returns a function the default is
#' splinefun. Other possible values are the linear interpolation approxfun or
#' any self made function with the same interface.
#' @param format a string that specifies the format used to represent the
#' atmospheric fraction. Possible values are "Delta14C" which is the default or
#' "afn" the Absolute Fraction Normal representation
#' @return An object of the new class BoundFc that replaces FcAtm
FcAtm.from.Dataframe=function
(dframe, 
lag=0, 
interpolation=splinefun, 
format 
){
   warning("The class FcAtm is deprecated, you can use the generic constructor BoundFc with the same data.frame arguemten instead")
   obj=BoundFc(dframe,lag=lag,format=format) 
return(obj)
}
