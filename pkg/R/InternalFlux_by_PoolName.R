#' constructor from an ordered pair of PoolName (string like) objects and a function with the set of formal argument names forming a 
#' subset of the state_variable_names
#' 
#' @param func A real valued function describing the flux (mass/time)
#' as function of the state variables and time.  
#' @param sourceName A string identifying the source pool of the flux
#' @param destinationName A string identifying the destination pool of the flux
setMethod(
  f="InternalFlux_by_PoolName",
  signature=c(
    func='function'
    ,sourceName='character'
    ,destinationName='character'
    ,src_to_dest='missing'
  ),
  def=function(
    func
    ,sourceName
    ,destinationName
    ){
    new(
        'InternalFlux_by_PoolName'
        ,func=func
        ,sourceName=PoolName(sourceName)
        ,destinationName=PoolName(destinationName)
    )
  }
)

#' automatic title
#' 
#' @param func : no manual documentation
#' @param src_to_dest : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
  f="InternalFlux_by_PoolName",
  signature=c(
    func='function'
    ,sourceName='missing'
    ,destinationName='missing'
    ,src_to_dest='character'
  ),
  def=function(
    func
    ,src_to_dest
    ){
      new(
          "InternalFlux_by_PoolName"
          ,sourceName=getSenderName(src_to_dest)
          ,destinationName=getRecipientName(src_to_dest)
          ,func=func
     )
  }
)

#' automatic title
#' 
#' @param obj : no manual documentation
#' @param poolNames : no manual documentation
#' @param timeSymbol : no manual documentation
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
        obj='InternalFlux_by_PoolName'
        ,poolNames='character'
        ,timeSymbol='character'
    )
    ,definition=function(obj,poolNames,timeSymbol){
        fl_by_index<-new(
            "InternalFlux_by_PoolIndex"
            ,sourceIndex=PoolIndex(
                obj@sourceName
                ,poolNames
            )
            ,destinationIndex=PoolIndex(
                obj@destinationName
                ,poolNames
            )
            ,func=by_PoolIndex(
                obj@func
                ,timeSymbol=timeSymbol
                ,poolNames=poolNames
            )
        )
        fl_by_index
    }
)
