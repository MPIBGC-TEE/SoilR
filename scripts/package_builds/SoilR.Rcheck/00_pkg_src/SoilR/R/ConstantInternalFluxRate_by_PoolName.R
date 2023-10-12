#' constructor with argument conversion 
#' @param sourceName name of source pool
#' @param destinationName name of destination pool
#' @param rate_constant a value of the rate constant
setMethod(
    f='ConstantInternalFluxRate_by_PoolName'
    ,signature=signature(
        sourceName='character'
        ,destinationName='character'
        ,src_to_dest='missing'
        ,rate_constan='numeric'
    )
    ,definition=function(sourceName,destinationName,rate_constant){
        new(
          'ConstantInternalFluxRate_by_PoolName'
          ,sourceName=PoolName(sourceName)
          ,destinationName=PoolName(destinationName)
          ,rate_constant=rate_constant
        )
  }
)

#' constructor from strings of the form 'a->b'
#' @param src_to_dest a string indicating the source to destination pools
#' @param rate_constant a value of the rate constant 
setMethod(
  f="ConstantInternalFluxRate_by_PoolName",
  signature=signature(
     sourceName='missing'
    ,destinationName='missing'
    ,src_to_dest='character'
    ,rate_constant='numeric'
  ),
  def=function(src_to_dest,rate_constant){
      new(
          "ConstantInternalFluxRate_by_PoolName"
          ,sourceName=getSenderName(src_to_dest)
          ,destinationName=getRecipientName(src_to_dest)
          ,rate_constant=rate_constant
      )
  }
)

#' new object with the source pool id converted to a PoolName if necessary 
#' @param obj an object of class ConstantInternalFluxRate_by_PoolName
#' @param poolNames names of the pools
setMethod(
  f="by_PoolIndex",
  signature=c(obj='ConstantInternalFluxRate_by_PoolName'),
  def=function(obj,poolNames){
      new(
        "ConstantInternalFluxRate_by_PoolIndex"
        ,sourceIndex=PoolIndex(id=obj@sourceName,poolNames)
        ,destinationIndex=PoolIndex(id=obj@destinationName,poolNames)
        ,rate_constant   =   obj@rate_constant
      )
  }
)
