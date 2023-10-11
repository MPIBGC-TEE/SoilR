#' A list of outfluxes 
#' 
#' @autocomment 
#' @s4superclasses
#' @s4subclasses
#' @s4methods
setClass(
  Class = "OutFluxList_by_PoolIndex",
  contains=c("list")
)

#' constructor from a normal list
#'
#' after checking the elements
#' @param object a list
setMethod(
    "OutFluxList_by_PoolIndex"
    ,signature=signature(object="list")
    ,definition=function(object){
        checkTargetClassOfElements(object,targetClassName='OutFlux_by_PoolIndex')
        as(object,'OutFluxList_by_PoolIndex')
    }
)
