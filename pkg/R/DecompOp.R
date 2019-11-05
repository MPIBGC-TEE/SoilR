


setMethod(
  f="GeneralDecompOp",
  signature=signature(object="DecompOp"),
  def=function 
  (object){
    object
  }
)



setMethod(
  "GeneralDecompOp",
  signature(object="matrix"),
  def=function 
  (object){
    ConstLinDecompOp(object)
  }
)



setMethod(
  f="GeneralDecompOp",
  signature=signature(object="TimeMap"),
  def=function 
  (object){
    BoundLinDecompOp(object)
  }
)
