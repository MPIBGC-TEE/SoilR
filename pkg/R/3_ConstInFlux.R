#
# vim:set ff=unix expandtab ts=2 sw=2:
#-----------------------------------------------------------------------------------

### defines a constant inputrate 
setClass(
   Class="ConstInFlux",
   contains=c("InFlux"),
   slots=list(
    map="numeric"
   )
)
##------------------------------ constructors------------------------------------------
#setMethod(
#    f="initialize",
#    signature="ConstInFlux",
#    definition=function # internal constructor
#    ### This mehtod is intended for internal use only, it may change with the internal representation of the class. In user code please use the generic constructor \code{\link{ConstInFlux}} instead.
#    (.Object,starttime=numeric(),endtime=numeric(),map=function(t){t},lag=0){
#    #cat("-initializer at work-\n")
#    .Object@starttime=starttime
#    .Object@endtime=endtime
#    .Object@map=map
#    .Object@lag=lag
#    return(.Object)
#    }
#)
setMethod(
  f="ConstInFlux",
  signature=c(
    map="numeric"
  ),
  definition=function # constructor 
  ### the method converts a vector of constant input rates into an object of Class
  ### \code{\link{ConstInFlux}}.
  (
    map
    ){
    new("ConstInFlux",map=map)
  }
)

##-------------------------------------------other methods---------------------------------------------------
setMethod(
    f="getTimeRange",
    signature="ConstInFlux",
    definition=function # time domain of the function
    ### The method returns a vector containing the start and end time where the intepolation is valid. Since the class \code{\link{ConstInFlux}} represents an input stream
    ### constant in time it will return -infinity,+infinity
 
    ( object){
        return(
               c("t_min"=-Inf,"t_max"=Inf))
    }
)
setMethod(
    f="getFunctionDefinition",
    signature="ConstInFlux",
    definition=function(object){
    ### create the (constant) function of time that is required by the models 
        return(function(t){object@map})
    ##value<< A constant function of time that is used by the Models to represent the input fluxes.
    }
)
