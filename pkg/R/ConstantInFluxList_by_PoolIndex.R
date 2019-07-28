#' constructor from a normal list 
#' 
#' @param l A list. Either a list of elements of type  
#' \linkS4class{ConstantInFlux} or 
#' a list where the names of the elements are strings of the form
#' '1->3' (for the flux rate from pool 1 to 2
#'
#' @return An object of class
#' \linkS4class{ConstantInFluxList_by_PoolIndex} 
#' 
#' 
#' The function checks if the elements are of the desired type or can be
#' converted to it. It is mainly used internally and usually called 
#' by the front end functions to convert the user supplied arguments.

setMethod("ConstantInFluxList_by_PoolIndex",
    signature=signature(object="list"),
    definition=function(object){
        makeListInstance(
            object
            ,targetClassName='ConstantInFlux_by_PoolIndex'
            ,targetListClassName="ConstantInFluxList_by_PoolIndex"
            ,permittedValueClassName='numeric'
            ,key_value_func=function(key,val){
                ConstantInFlux_by_PoolIndex(
                    destinationIndex=PoolIndex(key)
                    ,flux_constant=object[[key]]
                )
            }
        )
    }
)

setMethod("ConstantInFluxList_by_PoolIndex",
    signature=signature(object="numeric"),
    definition=function(object){
        ConstantInFluxList_by_PoolIndex(as.list(object))
    }
)

#' constructor from ConstInFluxes 
#' 
#' @param object An object of class \linkS4class{ConstInFluxes}
#' @return An object of class \linkS4class{ConstantInFluxList_by_PoolIndex}
setMethod(
    "ConstantInFluxList_by_PoolIndex"
    ,signature=signature(
         object='ConstInFluxes'
    )
    ,def=function(object){
        vec=getConstantInFluxVector(object)
        non_zero_positions=(1:length(vec))[vec!=0]
        l=lapply(
            non_zero_positions
            ,function(pos){
                ConstantInFlux_by_PoolIndex(
                    destinationIndex=pos
                    ,flux_constant=vec[[pos]]
                )
            }
        )
        as(l,'ConstantInFluxList_by_PoolIndex')
    }
)

##' convert to a list indexed by pool names
##'
#setMethod("by_PoolName",
#    signature=signature(obj="ConstantInFluxList_by_PoolIndex"),
#    definition=function(obj,poolNames){
#        l=lapply(
#                obj
#                ,function(rate){
#                    by_PoolName(rate,poolNames)
#                }
#        )
#        as(l,'ConstantInFluxList_by_PoolName')
#    }
#)

