#' Pass through factory for objects of subclasses of \linkS4class{DecompOp} 
#'
#' This method takes and returns an (identical) object that inherits 
#' from \linkS4class{DecompOp}.
#' It's purpose it to be able to call the generic function on arguments that are already
# of class \linkS4clas{DecompOp} without having to check if this is necessary.  
#' @param object An object that already is of class \code{DecompOp}
#' @autocomment 
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
#' @param object no manual documentation
#' @autocomment 
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
#' @param object no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
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
