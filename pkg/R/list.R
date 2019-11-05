


setMethod(
  f="InFluxes",
  signature=signature(object="list"),
  def=function 
  (object){
    BoundInFluxes(TimeMap(object))
  }
)



setMethod(
  f="GeneralDecompOp",
  signature=signature(object="list"),
  def=function 
  (object){
    BoundLinDecompOp(TimeMap(object))
  }
)
