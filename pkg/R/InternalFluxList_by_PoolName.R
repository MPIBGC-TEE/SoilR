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
#' @param obj no manual documentation
#' @param poolNames no manual documentation
#' @param timeSymbol no manual documentation
#' @autocomment 
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

#' Convert to a numeric vector with names of the form 'a->b'
#'
#' @template FluxListAsNumeric
#' @param ... additional arguments
setMethod("as.numeric",
    signature(x = "InternalFluxList_by_PoolName"),
    function (x,y,t,time_symbol,...) {
      num_fluxes <-as.numeric(
        lapply(
          x,
          function(flux){
            apply_to_state_vec_and_time(flux@func,y,t,time_symbol)
          }
        )
      )
      names(num_fluxes) <- as.character(
        lapply(
          x,
          function(flux){
            src_to_dest_string(
              flux@sourceName,
              flux@destinationName
            )
          }
        )
      )
      num_fluxes
    }
)
