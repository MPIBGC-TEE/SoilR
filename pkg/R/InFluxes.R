#' automatic title
#' 
#' @param object no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
  f="InFluxes",
  signature(object="TimeMap"),
  def=function 
  (object)
  {
    BoundInFluxes(object)
  }
)

#' automatic title
#' 
#' @param object no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
  f="InFluxes",
  signature=signature(object="InFluxes"),
  def=function 
  (object){
    object
  }
)

#' automatic title
#' 
#' @param object no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
  f="InFluxes",
  signature=signature(object="numeric"),
  def=function 
  (object){
    ConstInFluxes(object)
  }
)

#' automatic title
#' 
#' @param object no manual documentation
#' @param numberOfPools no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
  f="InFluxes",
  signature=signature(object="ConstantInFluxList_by_PoolIndex"),
  def=function(object,numberOfPools){
    ConstInFluxes(map=object,numberOfPools=numberOfPools)
  }
)

#' automatic title
#' 
#' @param object no manual documentation
#' @param numberOfPools no manual documentation
#' basically produces a vector valued function from a list of scalar functions
setMethod(
  f="InFluxes",
  signature=signature(object="StateIndependentInFluxList_by_PoolIndex"),
  def=function(object,numberOfPools){

    vecFunc=function(t){
      res=matrix(nrow=numberOfPools,0)
      for (f in object){
        fluxFunc=getFunctionDefinition(f@flux)
        res[[f@destinationIndex]]<-fluxFunc(t)
      }
      res
    }
    # determine the time domain of the vecFunc
    t_mins=sapply(
      object,
      function(f){getTimeRange(f@flux)[[1]]}
    )
    t_maxs=sapply(
      object,
      function(f){getTimeRange(f@flux)[[2]]}
    )
    BoundInFluxes(
      map=vecFunc,
      starttime=max(t_mins),
      endtime=min(t_maxs)
    )
  }
)
