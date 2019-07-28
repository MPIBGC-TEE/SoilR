setClass(
  Class = "InFluxList_by_PoolName",
  contains=c("list")
)

#' constructor from a normal list
#'
#' after checking the elememts
setMethod("InFluxList_by_PoolName",
    signature=signature(object="list"),
    definition=function(object){
        checkTargetClassOfElements(object,targetClassName='InFlux_by_PoolName')
        as(object,'InFluxList_by_PoolName')
    }
)
setMethod(
    f="by_PoolIndex"
    ,signature=signature(
        obj='InFluxList_by_PoolName'
        ,timeSymbol='character'
        ,poolNames="character"
    )
    ,definition=function(obj,poolNames,timeSymbol){
        l<-lapply(
            obj
            ,function(fl_by_name){
                by_PoolIndex(obj=fl_by_name,poolNames=poolNames,timeSymbol=timeSymbol)
            }
        )
        as(l,'InFluxList_by_PoolIndex')
    }
)
