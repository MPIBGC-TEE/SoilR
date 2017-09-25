#
# vim:set ff=unix expandtab ts=2 sw=2:

    ### The decomposition operator is a necessary ingredient of any Model.
    ### Very generally it describes the fluxes between the pools and to the exterior 
    ### as functions of time and the pool contents. 
    ### SoilR arranges different  decomposition operators into  different classes, 
    ### to determine which computations can be performed with them (which methods can be called on them).
    ### The simplest and least general decomposition operator is \code{\link{ConstLinDecompOp-class}}
    ### which can be created from a constant reservoir matrix. 
    ### Since it is the least general (most specific) the additional information can be used to compute more 
    ### and more specific results (more methods)
    ### not available for the more abstract sub classes (for instance \code{\link{BoundLinDecompOp-class}} 
    ### that are necessary to model the more general situations where the decomposition and 
    ### transfer rates are functions of time.
    ### The different decomposition operators are created by class-specific functions, 
    ### called constructors. (see the constructors section of this help page)
setClass(# decomposition operator 
    Class="DecompOp",
    ,
    contains="VIRTUAL"
)
setMethod(
  f="GeneralDecompOp",
  signature=signature(object="DecompOp"),
  def=function # pass through factory 
  ### This method handles the case that no actual construction is necessary since
  ### the argument is already of a subclass of DecompOp 
  ##<<details This is useful to simplify argument handling of functions which rely on 
  ## the presence of a DecompOp. 
  ## Due to this method those functions can always
  ## call GeneralDecompOp(something) without having to check if 
  ## it is necessary.
  (object){
    object
    ### the unchanged argument
  }
)
setMethod(
  "GeneralDecompOp",
  signature(object="matrix"),
  #valueClass="ConstLinDecompOp",
  def=function # creates a ConstanDecompOp from a matrix
  ### The resulting operator is creted by a call to the constructor of class
  ### ConstLinDecompOp
  (object){
    ConstLinDecompOp(object)
  }
)
setMethod(
  f="GeneralDecompOp",
  signature=signature(object="TimeMap"),
  #valueClass="BoundLinDecompOp",
  def=function # creates a BoundLinDecompOp from a TimeMap object
  ### The resulting operator is creted by a call to the constructor of class
  ### BoundLinDecompOp
  ### The method is used to ensure backward compatibility with the now deprecated
  ### TimeMap class
  (object){
    BoundLinDecompOp(object)
  }
)
