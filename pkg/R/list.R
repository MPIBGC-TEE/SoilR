


#' Automatic description: InFluxes,list-method
#' 
#' @name InFluxes,list-method
#' @param object : object of class:\code{list}, no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
  f="InFluxes",
  signature=signature(object="list"),
  def=function 
  (object){
    BoundInFluxes(TimeMap(object))
  }
)



#' Automatic description: GeneralDecompOp,list-method
#' 
#' @name GeneralDecompOp,list-method
#' @param object : object of class:\code{list}, no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
  f="GeneralDecompOp",
  signature=signature(object="list"),
  def=function 
  (object){
    BoundLinDecompOp(TimeMap(object))
  }
)
