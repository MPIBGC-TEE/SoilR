#' constructor from a normal list 
#' 
#' @param object A list. Either a list of elements of type  
#' \linkS4class{InternalFlux_by_PoolName} or 
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

setMethod("InternalFluxList_by_PoolName",
    signature=signature(object="list"),
    definition=function(object){
        makeListInstance(
            object
            ,targetClassName='InternalFlux_by_PoolName'
            ,targetListClassName="InternalFluxList_by_PoolName"
            ,permittedValueClassName='function'
            ,key_value_func=function(key,val){
                InternalFlux_by_PoolName(
                     src_to_dest=key
                    ,func=val
                )
            }
        )
    }
)

#' automatic title
#' 
#' @autocomment These comments were created by the auto_comment_roclet by
#' @param obj : : no manual documentation
#' @param poolNames : : no manual documentation
#' @param timeSymbol : : no manual documentation
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
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
