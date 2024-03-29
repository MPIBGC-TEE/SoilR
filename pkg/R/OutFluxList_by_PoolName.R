#' constructor from a normal list 
#' 
#' @param object A list. Either a list of elements of type  
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





#' automatic title
#' 
#' @param obj no manual documentation
#' @param poolNames no manual documentation
#' @param timeSymbol no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
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

#' Convert to a numeric vector with the pool names as names
#'
#' @template FluxListAsNumeric
#' @param ... additional arguments
setMethod("as.numeric",
    signature(x = "OutFluxList_by_PoolName"),
    function (x,y,t,time_symbol,...) {
      num_fluxes <-as.numeric(
        lapply(
          x,
          function(flux){
            apply_to_state_vec_and_time(flux@func,y,t,time_symbol)
          }
        )
      )
      names(num_fluxes) <- as.character(lapply(
        x,
        function(flux) flux@sourceName
      ))
      num_fluxes
    }
)
