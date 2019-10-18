
#' @auto

#' @auto

#' @auto
setMethod(
    'ConstantOutFluxRate_by_PoolIndex'
    ,signature=signature(
         sourceIndex='numeric'   # this also covers the preferred argument class 'PoolIndex' since it contains integer and so also numeric
        ,rate_constant='numeric'
    )
    ,def=function(sourceIndex,rate_constant){
        if (rate_constant<0){
          stop(
            "Negative rate constant. 
            A rate_constant defines a flux F with F = rate_constant*pool_content. 
            Since fluxes have to be positive and pool contents are positive
            rate constants have to be positive too."
          )
        }
        rate=new(
            'ConstantOutFluxRate_by_PoolIndex'
            ,sourceIndex=PoolIndex(id=sourceIndex)
            ,rate_constant=rate_constant
        )
        rate
    }
)

#' new object with the source pool id converted to a PoolName if necessary 
#' 
#' This method exists only for classes that do not contain functions of 
#' the state_variables since we cannot automatically translate functions 
#' with a state vector arguments to functions of the respective state variables
#' which would require symbolic computations.
#' The reverse direction is always possible and is therefore 
#' the preferred way to input 
#' rate functions that depend on state variables.
setMethod(
  f="by_PoolName",
  signature=c(obj='ConstantOutFluxRate_by_PoolIndex'),
  def=function(obj,poolNames){
      new(
        'ConstantOutFluxRate_by_PoolName'
        ,sourceName=PoolName(id=obj@sourceIndex,poolNames)
        ,rate_constant=obj@rate_constant
      )
  }
)
