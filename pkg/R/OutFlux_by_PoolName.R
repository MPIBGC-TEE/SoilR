#' constructor from a PoolName (integer like) object and a function 
#' @param func A function. The names of the formal aguemnts have to be a subset of the state variable names and the time symbol 
#' This allows subsequent automatic reordering of the state variables.
#' In the presence of a vector of stave variabl names the formulation can 
#' automatically be transformed to a function of a s tate VECTOR argument and #' time

#' constructor from an ordered pair of PoolName (integer like) objects 
setMethod(
  f="OutFlux_by_PoolName",
  signature=c(
    func='function'
    ,sourceName='character'
  )
  ,def=function(func,sourceName){
    new(
        'OutFlux_by_PoolName'
        ,sourceName=PoolName(sourceName)
        ,func=func
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
        obj='OutFlux_by_PoolName'
        ,poolNames='character'
        ,timeSymbol='character'
    )
    ,definition=function(obj,poolNames,timeSymbol){
        fl_by_index<-new(
            "OutFlux_by_PoolIndex"
            ,sourceIndex=PoolIndex(
                obj@sourceName
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
