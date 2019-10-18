
#' @auto

#' @auto

#' @auto
setMethod(
  f="ConstInFluxes",
  signature=c(
    map="numeric"
  ),
  definition=function 
  (
    map
    ){
    new("ConstInFluxes",map=map)
  }
)

#' @auto

#' @auto

#' @auto
setMethod(
    "ConstInFluxes"
    ,signature=signature(
         map='ConstantInFluxList_by_PoolIndex'
        ,numberOfPools='numeric'
    )
    ,def=function(map,numberOfPools){
        res=rep(0,numberOfPools)
        for (f in map){res[[f@destinationIndex]]<-f@flux_constant}
        ConstInFluxes(map=res)
    }
)

#' @auto

#' @auto

#' @auto
setMethod(
    "getConstantInFluxVector"
    ,signature=signature(
         object='ConstInFluxes'
    )
    ,def=function(object){
        object@map
    }
)

#' @auto

#' @auto

#' @auto
setMethod(
    f="getTimeRange",
    signature="ConstInFluxes",
    definition=function 
    ( object){
        return(
               c("t_min"=-Inf,"t_max"=Inf))
    }
)

#' @auto

#' @auto

#' @auto
setMethod(
    f="getFunctionDefinition",
    signature="ConstInFluxes",
    definition=function(object){
        return(function(t){matrix(ncol=1,object@map)})
    }
)

