setClass(
  Class="ConstantInFlux_by_PoolIndex",
  slots=c(destinationIndex='PoolIndex',flux_constant='numeric')
)

ConstantInFlux_by_PoolIndex<-function(destinationIndex,flux_constant){
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
