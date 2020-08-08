#' constructor from a normal list
#'
#' after checking the elements

setMethod("InFluxList_by_PoolName",
    signature=signature(object="list"),
    definition=function(object){
        makeListInstance(
            object
            ,targetClassName='InFlux_by_PoolName'
            ,targetListClassName="InFluxList_by_PoolName"
            # fixme mm: 08/08/2020
            # the function should also be able to convert constant
            # InFluxes among the list elements
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




#' Transform pool names to indices
#' 
#' @param obj no manual documentation
#' @param poolNames no manual documentation
#' @param timeSymbol no manual documentation
#' @autocomment 
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



#' automatic title
#' 
#' @param object no manual documentation
#' @param timeSymbol no manual documentation
#' @param poolNames no manual documentation
#' @autocomment 
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

#' Convert to a numeric vector with the pool names as names
#'
#' @template FluxListAsNumeric
setMethod("as.numeric",
    signature(x = "InFluxList_by_PoolName"),
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
        function(flux) flux@destinationName
      ))
      num_fluxes
    }
)
