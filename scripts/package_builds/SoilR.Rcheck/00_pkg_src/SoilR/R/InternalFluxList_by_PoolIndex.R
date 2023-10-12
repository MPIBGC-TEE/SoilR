#' Constructor from a normal list
#' after checking the elements
#' 
#' @param object a list
setMethod("InternalFluxList_by_PoolIndex",
    signature=signature(object="list"),
    definition=function(object){
        checkTargetClassOfElements(object,targetClassName='InternalFlux_by_PoolIndex')
        as(object,'InternalFluxList_by_PoolIndex')
    }
)
