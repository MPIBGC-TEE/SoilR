
# vim:set ff=unix expandtab ts=2 sw=2:
#-----------------------------------------------------------
setMethod(
  f="InFlux",
  signature=signature(object="list"),
  def=function # creates a BoundInFlux from a nested list of a times vector and a list of vectors (a vector for each time step)
  ### The resulting object is created by a call to the constructor of class
  ### BoundInFlux
  (object){
    BoundInFlux(TimeMap(object))
  }
)
#-----------------------------------------------------------
setMethod(
  f="GeneralDecompOp",
  signature=signature(object="list"),
  def=function # creates a BoundLinDecompOp from a nested list of a times vector  and a  list of matrices (a matrix for each time step)
  ### The resulting operator is creted by a call to the constructor of class
  ### BoundLinDecompOp

  (object){
    BoundLinDecompOp(TimeMap(object))
  }
)
