#' constructor from a normal list 
#' 
#' @param l A list. Either a list of elements of type  
#' \linkS4class{OutFlux_by_PoolName} or 
#' a list where the names of the elements are integer strings.
#'
#' @return An object of class
#' \linkS4class{ConstantInFluxList_by_PoolIndex} 
#' 
#' 
#' The function checks if the elements are of the desired type or can be
#' converted to it. It is mainly used internally and usually called 
#' by the front end functions to convert the user supplied arguments.

setMethod("OutFluxList_by_PoolName",
    signature=signature(object="list"),
    definition=function(object){
        makeListInstance(
            object
            ,targetClassName='OutFlux_by_PoolName'
            ,targetListClassName="OutFluxList_by_PoolName"
            ,permittedValueClassName='function'
            ,key_value_func=function(key,val){
                OutFlux_by_PoolName(
                    sourceName=key
                    ,func=val
                )
            }
        )
    }
)



#' @auto

#' @auto

#' @auto
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
                by_PoolIndex(obj=fl_by_name,poolNames=poolNames,timeSymbol=timeSymbol)
            }
        )
        as(l,'OutFluxList_by_PoolIndex')
    }
)
