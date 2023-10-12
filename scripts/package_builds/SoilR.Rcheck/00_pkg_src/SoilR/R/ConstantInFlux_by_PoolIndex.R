ConstantInFlux_by_PoolIndex<-function(
    destinationIndex
    ,flux_constant
){
    if (flux_constant<0){
      stop(
        paste(
            "Fluxes have to be positive.  Got Negative flux to pool:"
            ,destinationIndex
            , "with flux_constant:"
            ,flux_constant
        ) 
      )
    }
    new(
        'ConstantInFlux_by_PoolIndex'
        ,destinationIndex=PoolIndex(destinationIndex)
        ,flux_constant=flux_constant
    )    
    
}

#' new object with the source pool id converted to a PoolIndex if necessary 
#' @param obj an object of class ConstantInFlux_by_PoolIndex
#' @param poolNames names of pools
setMethod(
    f="by_PoolName",
    signature=c(obj='ConstantInFlux_by_PoolIndex'),
    def=function(obj,poolNames){
        new(
            "ConstantInFlux_by_PoolName"
            ,destinationName=PoolName(id=obj@destinationIndex ,poolNames)
            ,flux_constant=obj@flux_constant
        )
    }
)
