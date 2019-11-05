#' constructor from a normal list
#'
#' after checking the elememts

setMethod("InFluxList_by_PoolName",
    signature=signature(object="list"),
    definition=function(object){
        makeListInstance(
            object
            ,targetClassName='InFlux_by_PoolName'
            ,targetListClassName="InFluxList_by_PoolName"
            ,permittedValueClassName='function'
            ,key_value_func=function(key,val){
                InFlux_by_PoolName(
                     destinationName=key
                    ,func=val
                )
            }
        )
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



setMethod("getFunctionDefinition",
    signature=signature(object="InFluxList_by_PoolName"),
    definition=function(
        object
        ,timeSymbol
        ,poolNames
    ){
        o_by_Index=by_PoolIndex(object,timeSymbol=timeSymbol,poolNames=poolNames)
        getFunctionDefinition(o_by_Index,numberOfPools=length(poolNames))
    }
)
