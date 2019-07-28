setClass(
  Class="StateIndependentInFlux_by_PoolIndex",
  slots=c(destinationIndex='PoolIndex',flux='ScalarTimeMap')
)

StateIndependentInFlux_by_PoolIndex<-function(destinationIndex,flux){
    new(
        'StateIndependentInFlux_by_PoolIndex'
        ,destinationIndex=PoolIndex(destinationIndex)
        ,flux=flux
    )    
    
}
