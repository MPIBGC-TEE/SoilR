setClass(
    Class="UnBoundInFluxes",
    contains=c("InFluxes"),
    slots=list( map="function")
)



setMethod(
     f="initialize",
     signature="UnBoundInFluxes",
     definition=function 
     (.Object,map=function(){})
     {
        .Object@map=map
     return(.Object)
     }
)



setMethod(
      f="UnBoundInFluxes",
      signature=c(map="function"),
      definition=function 
      (map){
      return(new("UnBoundInFluxes",map=map))
     }
)



setMethod(
    f="getFunctionDefinition",
    signature="UnBoundInFluxes",
    definition=function 
    (object){
      return(object@map)
    }
)



setMethod(
    f="getTimeRange",
    signature="UnBoundInFluxes",
    definition=function 
    (object)
    {
        return( c("t_min"=-Inf,"t_max"=Inf))
    }
)
