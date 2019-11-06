#' Automatic description: InFluxes,TimeMap-method
#' 
#' @name InFluxes,TimeMap-method
#' @param object : object of class:\code{TimeMap}, no manual documentation
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

#' Automatic description: InFluxes,InFluxes-method
#' 
#' @name InFluxes,InFluxes-method
#' @param object : object of class:\code{InFluxes}, no manual documentation
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

#' Automatic description: InFluxes,numeric-method
#' 
#' @name InFluxes,numeric-method
#' @param object : object of class:\code{numeric}, no manual documentation
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

#' Automatic description: InFluxes,ConstantInFluxList_by_PoolIndex-method
#' 
#' @name InFluxes,ConstantInFluxList_by_PoolIndex-method
#' @param object : object of class:\code{ConstantInFluxList_by_PoolIndex}, no
#' manual documentation
#' @param numberOfPools : no manual documentation
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
