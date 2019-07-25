 setClass(
  Class="ConstantInternalFluxRate_by_PoolName",
  slots=c(sourceName="PoolName",destinationName='PoolName',rate_constant='numeric')
)

#' constructor with argument conversion 
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
