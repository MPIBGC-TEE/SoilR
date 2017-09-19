#
# vim:set ff=unix expandtab ts=2 sw=2:
 SoilR.F0.new <- function # deprecated function that used to create an object of class SoilR.F0 
 ### The function internally calls the constructor of the replacement class \code{\link{ConstFc-class}}.

     (
     values=c(0),  ##<< a numeric vector
     format="Delta14C"   ##<< a character string describing the format e.g. "Delta14C"
     )
     {
      warning(WarningConstFc())
 	F0=ConstFc(values=values,format=format)
 	return(F0)
 	### An object of class \code{\link{ConstFc-class}} that contains data and a format description that can later be used to convert the data into other formats if the conversion is implemented.
 }
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
