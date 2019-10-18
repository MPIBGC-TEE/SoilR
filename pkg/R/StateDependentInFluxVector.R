
#' @auto

#' @auto

#' @auto
setMethod(
    'getFunctionDefinition'
    ,signature=signature(object='StateDependentInFluxVector')
    ,definition=function(object){
        object@map
    }
)
