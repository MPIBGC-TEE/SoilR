setClass(
  Class = "InternalFluxList_by_PoolName",
  contains=c("list")
)

#' constructor from a normal list
#'
#' after checking the elememts
setMethod("InternalFluxList_by_PoolName",
    signature=signature(l="list"),
    function (l){
    checkTargetClassOfElements(l,targetClassName='InternalFlux_by_PoolName')
        as(l,'InternalFluxList_by_PoolName')

    }
)

setMethod(
    f="by_PoolIndex"
    ,signature=signature(
        obj='InternalFluxList_by_PoolName'
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
        as(l,'InternalFluxList_by_PoolIndex')
    }
)
