setClass(
  Class = "InFlux_by_PoolName",
  # contains="PoolConnection", we do not want to iherit mehtods...
  slots=c(destinationName='PoolName',func='function')
)

#' constructor from a PoolName (integer like) object and a function 
#' @param func A function. The names of the formal aguemnts have to be a subset of the state variable names and the time symbol 
#' This allows subsequent automatic reordering of the state variables.
#' In the presence of a vector of stave variabl names the formulation can 
#' automatically be transformed to a function of a s tate VECTOR argument and #' time

#' constructor from an ordered pair of PoolName (integer like) objects 
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
