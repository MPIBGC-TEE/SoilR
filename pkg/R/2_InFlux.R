#
# vim:set ff=unix expandtab ts=2 sw=2:
setClass(
   Class="InFlux",
   contains="VIRTUAL"
)
setMethod(
  f="InFlux",
  signature(object="TimeMap"),
  def=function #create a BoundInFlux from a TimeMap object
  ### The method is used to ensure backward compatibility with the now deprecated
  ### TimeMap class.
  ### The resulting BoundInFlux is created by a call to
  ### the constructor BoundInFlux(object) of that class.
  (object)
  {
    BoundInFlux(object)
  }

)
setMethod(
  f="InFlux",
  signature=signature(object="InFlux"),
  def=function # pass through conversion
  ### This method handles the case that no actual conversion is necessary since
  ### the argument is already of a subclass of InFlux 
  ##<<details This is useful to simplify argument handling of functions which rely on 
  ## the presence of some kind of an InFlux. 
  ## Due to this method those functions can 
  ## call InFlux(something) without having to check if 
  ## it is necessary.
  (object){
    object
    ### the unchanged argument
  }
)
setMethod(
  f="InFlux",
  signature=signature(object="numeric"),
  def=function # conversion of a vector to an object of class \code{\link{ConstInFlux}}
  ### This method enables the model creating functions to handle constant input streams 
  ## simply given as a vector.
 
  (object){
    ConstInFlux(object)
    ### an object of class \code{\link{ConstInFlux}} that can be used in model craeating Functions
  }
)
