#' constructor from a PoolName (integer like) object and a function 
#' @param func A function. The names of the formal aguemnts have to be a subset of the state variable names and the time symbol 
#' This allows subsequent automatic reordering of the state variables.
#' In the presence of a vector of stave variabl names the formulation can 
#' automatically be transformed to a function of a s tate VECTOR argument and #' time

#' constructor from an ordered pair of PoolName (integer like) objects 
setMethod(
  f="OutFlux_by_PoolName",
  signature=c(
    func='function'
    ,sourceName='character'
  )
  ,def=function(func,sourceName){
    new(
        'OutFlux_by_PoolName'
        ,sourceName=PoolName(sourceName)
        ,func=func
    )
  }
)

#' @auto

#' @auto

#' @auto
setMethod(
    f="by_PoolIndex"
    ,signature=signature(
        obj='OutFlux_by_PoolName'
        ,poolNames='character'
        ,timeSymbol='character'
    )
    ,definition=function(obj,poolNames,timeSymbol){
        fl_by_index<-new(
            "OutFlux_by_PoolIndex"
            ,sourceIndex=PoolIndex(
                obj@sourceName
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
