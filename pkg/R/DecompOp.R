
#' @auto

#' @auto

#' @auto
setMethod(
  f="GeneralDecompOp",
  signature=signature(object="DecompOp"),
  def=function 
  (object){
    object
  }
)

#' @auto

#' @auto

#' @auto
setMethod(
  "GeneralDecompOp",
  signature(object="matrix"),
  def=function 
  (object){
    ConstLinDecompOp(object)
  }
)

#' @auto

#' @auto

#' @auto
setMethod(
  f="GeneralDecompOp",
  signature=signature(object="TimeMap"),
  def=function 
  (object){
    BoundLinDecompOp(object)
  }
)
