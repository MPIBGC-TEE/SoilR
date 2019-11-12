#' Constructor from an ordered pair of PoolName (string like) and function  objects 
#'
#' @param destinationName PoolName (string like) object and a function 
#' @param func A function. The names of the formal arguments have to be a subset of the state variable names and the time symbol 
#' This allows subsequent automatic reordering of the state variables.
#' In the presence of a vector of state-variable-names the formulation can 
#' automatically be transformed to a function of a s tate VECTOR argument and 
#' time
setMethod(
  f="InFlux_by_PoolName",
  signature=c(
    func='function'
    ,destinationName='character'
  )
  ,def=function(func,destinationName){
    new(
        'InFlux_by_PoolName'
        ,destinationName=PoolName(destinationName)
        ,func=func
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
        obj='InFlux_by_PoolName'
        ,poolNames='character'
        ,timeSymbol='character'
    )
    ,definition=function(obj,poolNames,timeSymbol){
        fl_by_index<-new(
            "InFlux_by_PoolIndex"
            ,destinationIndex=PoolIndex(
                obj@destinationName
                ,poolNames=poolNames
            )
            ,func=by_PoolIndex(
                obj@func
                ,poolNames=poolNames
                ,timeSymbol=timeSymbol
            )
        )
        fl_by_index
    }
)
