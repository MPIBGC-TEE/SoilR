


#' automatic title
#' 
#' @autocomment These comments were created by the auto_comment_roclet by
#' @param object : : no manual documentation
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
  f="GeneralDecompOp",
  signature=signature(object="DecompOp"),
  def=function 
  (object){
    object
  }
)



#' automatic title
#' 
#' @autocomment These comments were created by the auto_comment_roclet by
#' @param object : : no manual documentation
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
  "GeneralDecompOp",
  signature(object="matrix"),
  def=function 
  (object){
    ConstLinDecompOp(object)
  }
)



#' automatic title
#' 
#' @autocomment These comments were created by the auto_comment_roclet by
#' @param object : : no manual documentation
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
  f="GeneralDecompOp",
  signature=signature(object="TimeMap"),
  def=function 
  (object){
    BoundLinDecompOp(object)
  }
)
