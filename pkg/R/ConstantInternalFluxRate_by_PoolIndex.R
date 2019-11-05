# constructor from number like objects



setMethod(
    f='ConstantInternalFluxRate_by_PoolIndex'
    ,signature=signature(
        sourceIndex='numeric'
        ,destinationIndex='numeric'
        ,src_to_dest='missing'
        ,rate_constant='numeric'
    )
    ,definition=function(
        sourceIndex
        ,destinationIndex
        ,rate_constant
    ){
        new(
            'ConstantInternalFluxRate_by_PoolIndex'
            ,sourceIndex=PoolIndex(sourceIndex)
            ,destinationIndex=PoolIndex(destinationIndex)
            ,rate_constant=rate_constant
        )
    }
)

#' constructor from strings of the form '1_to_2'
setMethod(
  f="ConstantInternalFluxRate_by_PoolIndex",
  signature=signature(
    sourceIndex='missing'
    ,destinationIndex='missing'
    ,src_to_dest='character'
    ,rate_constant='numeric'
  ),
  def=function(src_to_dest,rate_constant){
      new(
          "ConstantInternalFluxRate_by_PoolIndex"
          ,sourceIndex=getSenderIndex(src_to_dest)
          ,destinationIndex=getRecipientIndex(src_to_dest)
          ,rate_constant=rate_constant
      )
  }
)

#' new object with the source pool id converted to a PoolIndex if necessary 
setMethod(
  f="by_PoolName",
  signature=c(obj='ConstantInternalFluxRate_by_PoolIndex'),
  def=function(obj,poolNames){
      new(
        'ConstantInternalFluxRate_by_PoolName'
        ,sourceName      =   PoolName(obj@sourceIndex,poolNames)
        ,destinationName =   PoolName(obj@destinationIndex,poolNames)
        ,rate_constant   =   obj@rate_constant
      )
  }
)
