
#' @auto

#' @auto

#' @auto
setMethod(
  f="InFluxes",
  signature=signature(object="list"),
  def=function 
  (object){
    BoundInFluxes(TimeMap(object))
  }
)

#' @auto

#' @auto

#' @auto
setMethod(
  f="GeneralDecompOp",
  signature=signature(object="list"),
  def=function 
  (object){
    BoundLinDecompOp(TimeMap(object))
  }
)
