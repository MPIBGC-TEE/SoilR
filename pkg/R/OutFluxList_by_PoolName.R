setClass(
  Class = "OutFluxList_by_PoolName",
  contains=c("list")
)

#' constructor from a normal list
#'
#' after checking the elememts
setMethod("OutFluxList_by_PoolName",
    signature=signature(l="list"),
    definition=function(l){
        checkTargetClassOfElements(l,targetClassName='OutFlux_by_PoolName')
        as(l,'OutFluxList_by_PoolName')
    }
)
setMethod(
    f="by_PoolIndex"
    ,signature=signature(
        obj='OutFluxList_by_PoolName'
        ,timeSymbol='character'
        ,poolNames="character"
    )
    ,definition=function(obj,poolNames,timeSymbol){
        l<-lapply(
            obj
            ,function(fl_by_name){
                print("##############################")
                print("##############################")
                print("##############################")
                print(fl_by_name)
                by_PoolIndex(obj=fl_by_name,poolNames=poolNames,timeSymbol=timeSymbol)
            }
        )
        as(l,'OutFluxList_by_PoolIndex')
    }
)
