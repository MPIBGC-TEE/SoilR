
# vim:set ff=unix expandtab ts=2 sw=2:
setMethod(
  f="GeneralDecompOp",
  signature=signature(object="function"),
  def=function # creates a UnBoundLinDecompOp from a matrix valued function  
  ### The resulting operator is created by a call to the constructor of class
  ### UnBoundLinDecompOp
  (object){
    UnBoundLinDecompOp(object)
  }
)
setMethod(
  f="GeneralInFlux",
  signature=signature(object="function"),
  def=function # creates a UnBoundInflux from a vector valued function  
  (object){
    UnBoundInFlux(object)
  }
)
