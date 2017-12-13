#
# vim:set ff=unix expandtab ts=2 sw=2:
setClass(# Objects containing the atmospheric 14C fraction and the format it is provided in. 
    ### Objects of this class contain a time dependent function describing the Atmospheric 
    ### \eqn{^{14}C}{14C} fraction and a format description, 
    ### that allows to use the numeric valuest to be interpreted correctly in subsequent computations.
    Class="BoundFc",
    contains=c("TimeMap","Fc")
)
#------------------------ Constructors ---------------------------------
setMethod(
  f="BoundFc",
  signature=signature(format='character'),
  ### The method constructs an object from a format string and other parameters which are passed on to
  ### \code{\link{TimeMap}}
  definition=function # A constructor
(
  format,     ##<< a string that specifies the format used to represent the atmospheric fraction. Possible values are "Delta14C" which is the default or "afn" the Absolute Fraction Normal representation 
	   ...    ##<< passed on to TimeMap
){
    obj <- as(TimeMap(...),"BoundFc")
    obj@format=format
    validObject(obj) # will recursively call validObject on the superclasses
    return(obj)
}
)

#---------------------------------------------------------------------------------------------------------
setMethod(
  f="BoundFc",
  signature=signature(format='missing'),
  ### The method constructs an object from a list
  ### which must contain a 
  definition=function # A constructor
(
   format, ##<< If this method is called the format argument
    ## was not present.
    # fixme:mm
    # If the 'format' argument is ommitted the code  still works
    # but the documentation is no longer working 
    # since the util::getSrcref will not find the source code.
    # This is due that it calls methods::unRematchDefinition
    # on the MethodDefinition before its starts looking.
    # and unRematchDefinition introduces the format argument
    # in the argument list of the function stored 
    # in the definition=function
    # This is weierd since our definition has to have a formal argument
    # that will never be present since the method will only be chosen
    # when it is 'missing'
    # (methods::method.skeleton shows the same weierd behavior )
    # A possible fix is to let linkeddocs find the source coude of our 
    # methods without relying on utils::getSrcRef
    
  ... ##<<passed on to TimeMap
){
    l <- list(...)
    obj <- as(TimeMap(...),"BoundFc")
    obj@format=l$map$format
    validObject(obj) # will recursively call validObject on the superclasses
    return(obj)
}
)
#BoundFc <- function # A constructor
#(
#  format='Delta14C',
#  ... ##<<passed on to TimeMap
#){
#   # l <- list(...)
#   # if(is.null(format)){
#   # }else{
#   #   if(is.null(map)){
#   #     stop('If "format" is not a parameter "map" must be list with a 
#   #          entry "format".')
#   #     }else{
#   #       if(! 'format' %in% names(l$map)){
#   #         stop('format has to be a name in map')
#   #       }else{
#   #         form <- map$format
#   #       }
#   #     }
#   # }
#    obj <- as(TimeMap(...),"BoundFc")
#    obj@format=format
#    validObject(obj) # will recursively call validObject on the superclasses
#    return(obj)
#}

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


