#
# vim:set ff=unix expandtab ts=2 sw=2:
#correctnessOfDecompOp=function(object){ A=object@mat}
  
setClass(# constant decomposition operator 
    Class="UnBoundInFlux",
    contains=c("InFlux"),
    slots=list( map="function")
)
setMethod(
     f="initialize",
     signature="UnBoundInFlux",
     definition=function #internal constructor 
     ### This mehtod is intendet to be used internally. It may change in the future.
     ### For user code it is recommended to use one of the generic constructor \code{UnBoundInFlux} instead.
     (.Object,map=function(){})
     {
        .Object@map=map
     return(.Object)
     }
)
############################constructors####################
setMethod(
      f="UnBoundInFlux",
      ### 
      signature=c(map="function"),
      definition=function # construct from matrix valued function
      ### This method creates a UnBoundInFlux from a vector valued function
      (map){
      return(new("UnBoundInFlux",map=map))
     }
)

############################methods####################
setMethod(
    f="getFunctionDefinition",
    signature="UnBoundInFlux",
    definition=function # creates a constant timedependent function and returns it
      ### The method creates a timedependent function from the existing matrix describing the operator 
    (object){
      return(object@map)
    }
)
setMethod(
    f="getTimeRange",
    signature="UnBoundInFlux",
    definition=function # return an (infinite) time range since the function is assumed to be valid for all times
    ### some functions dealing with DecompOps in general rely on this
    ### so we have to implement it even though the timerange is always the same: (-inf,inf)
    (object)
    {
        return( c("t_min"=-Inf,"t_max"=Inf))
    }
)
