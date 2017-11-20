#
# vim:set ff=unix expandtab ts=2 sw=2:
correctnessOfBoundFc=function#check for unreasonable parameters or unsupported formats
###  The atmospheric C14 data can be represented in more than one format 
###  The function checks if the user required format is supported at the moment
(object ##<< the object to be tested
)
{
   res=TRUE
   supported_formats=supported14CFractionFormats()
   f=object@format
#   print(paste("format=",f))
   if (!any(grepl(f,supported_formats))){
      err_str=cat("The required format:",f," describing the atmospheric c_14 fraction is not supported.\n 
   	     The supported formats are: ",supported_formats,". \n",sep="")
      stop(simpleError(err_str))
      return(res)
   }
}

#---------------------------------------------------------------------------------------------------------
setClass(# Objects containing the atmospheric 14C fraction and the format it is provided in. 
    ### Objects of this class contain a time dependent function describing the Atmospheric 
    ### \eqn{^{14}C}{14C} fraction and a format description, 
    ### that allows to use the numeric valuest to be interpreted correctly in subsequent computations.
    Class="BoundFc",
    contains=c("TimeMap","Fc"),
    slots=list(
      format="character" 
   )
)
#------------------------ Constructors ---------------------------------
setMethod(
  f="BoundFc",
  signature=signature(format='character'),
  ### The method constructs an object from a function a timerange where it is valid and a format  
  definition=function # A constructor
(
  format,     ##<< a string that specifies the format used to represent the atmospheric fraction. Possible values are "Delta14C" which is the default or "afn" the Absolute Fraction Normal representation 
  ...         ##<<  will be passed on to \code{\link{TimeMap}}
){
    obj <- as(TimeMap(...),"BoundFc")
    obj@format=format
    correctnessOfBoundFc(obj)
    return(obj)
}
)

setMethod(
    f="getFormat",
    signature="BoundFc",
    definition=function# extract the format string
    ### the function just yields the format as a string
	  (object ##<< object  containing imformation about the format that could be Delta14C or AFM (Absolute Fraction Modern) for instance
		){
        return(object@format)
    }
)
#---------------------------------------------------------------------------------------------------------
setMethod(
   f= "Delta14C",
   signature("BoundFc"),
   definition=function# convert to Absolute Fraction Normal values  
   ### convert object containing values in any supported format to the appropriate Absolute Fraction Modern values.
   (
    F ##<< object of containing the values in any format
	 ){
      f=F@format
            targetFormat="Delta14C"
            if (f==targetFormat){
	       # do nothing
	       return(F)
	    }
	    if (f=="AbsoluteFractionModern"){
	     f_afn=F@map
             f_d14C=function(t){
	         fd=Delta14C_from_AbsoluteFractionModern(f_afn(t))
	     return(fd)
	    }
	    D14C=F
	    D14C@map=f_d14C
	    D14C@format=targetFormat
	    return(D14C)
	    } 
      stop("conversion not implemented for this format")
    }	 
)
#---------------------------------------------------------------------------------------------------------
setMethod(
  f= "AbsoluteFractionModern",
      signature("BoundFc"),
      definition=function# convert to Absolute Fraction Normal values  
      ### convert a BoundFc object containing values in any supported format to the appropriate Absolute Fraction Modern values.
	    (F ##<< object containing the values in any format
	    ){
        f=F@format
              targetFormat="AbsoluteFractionModern"
              if (f==targetFormat){
	         # do nothing
	         return(F)
	      }
	      if (f=="Delta14C"){
	       f_d14C=F@map
               f_afn=function(t){
	           fprime=AbsoluteFractionModern_from_Delta14C(f_d14C(t))
	       return(fprime)
	       }
	       AFM_tm=F
	       AFM_tm@map=f_afn
	       AFM_tm@format=targetFormat
	       return(AFM_tm)
	      } 
            stop("conversion not implemented for this format")
     }	 
)


