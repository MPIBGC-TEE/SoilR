
#' @auto

#' @auto

#' @auto
setMethod(
  f="InFluxes",
  signature(object="TimeMap"),
  def=function 
  (object)
  {
    BoundInFluxes(object)
  }
)

#' @auto

#' @auto

#' @auto
setMethod(
  f="InFluxes",
  signature=signature(object="InFluxes"),
  def=function 
  (object){
    object
  }
)

#' @auto

#' @auto

#' @auto
setMethod(
  f="InFluxes",
  signature=signature(object="numeric"),
  def=function 
  (object){
    ConstInFluxes(object)
  }
)

#' @auto

#' @auto

#' @auto
setMethod(
  f="InFluxes",
  signature=signature(object="ConstantInFluxList_by_PoolIndex"),
  def=function(object,numberOfPools){
    ConstInFluxes(map=object,numberOfPools=numberOfPools)
  }
)
