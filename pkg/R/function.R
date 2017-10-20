
setMethod(
  f="GeneralDecompOp",
  signature=signature(object="function"),
  #valueClass="BoundLinDecompOp",
  def=function # creates a UnBoundLinDecompOp from a matrix valued function  
  ### The resulting operator is creted by a call to the constructor of class
  ### UnBoundLinDecompOp
  (object){
    UnBoundLinDecompOp(object)
  }
)
