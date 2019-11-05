#' Constructor from an ordered pair of PoolName (string like) and function  objects 
#'
#' @param destinationName PoolName (string like) object and a function 
#' @param func A function. The names of the formal arguments have to be a subset of the state variable names and the time symbol 
#' This allows subsequent automatic reordering of the state variables.
#' In the presence of a vector of state-variable-names the formulation can 
#' automatically be transformed to a function of a s tate VECTOR argument and 
#' time
setMethod(
  f="InFlux_by_PoolName",
  signature=c(
    func='function'
    ,destinationName='character'
  )
  ,def=function(func,destinationName){
    new(
        'InFlux_by_PoolName'
        ,destinationName=PoolName(destinationName)
        ,func=func
    )
  }
)

setMethod(
    f="by_PoolIndex"
    ,signature=signature(
        obj='InFlux_by_PoolName'
        ,poolNames='character'
        ,timeSymbol='character'
    )
    ,definition=function(obj,poolNames,timeSymbol){
        fl_by_index<-new(
            "InFlux_by_PoolIndex"
            ,destinationIndex=PoolIndex(
                obj@destinationName
                ,poolNames=poolNames
            )
            ,func=by_PoolIndex(
                obj@func
                ,poolNames=poolNames
                ,timeSymbol=timeSymbol
            )
        )
        fl_by_index
    }
)
