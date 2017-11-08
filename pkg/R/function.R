
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
  def=function # creates a UnBoundInFlux from a vector valued function  
  ### The resulting operator is created by a call to the constructor of class
  ### UnBoundInFlux. You should only use this if the domain of your function is the complete
  ### time axis (-Inf,+Inf).
  ### If your function has a finite domain create an object of class \code{\link{BoundInFlux-class}}  ### by calling \code{\link{BoundInFlux}}. This will activeate checks on that avoid 
  ### unintended extrapolation.
  (object){
    UnBoundInFlux(object)
  }
)
