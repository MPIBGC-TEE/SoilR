
#' Subclass of list that is guaranteed to contain only elements of type
#' \linkS4class{ConstantInternalFluxRate_by_PoolName}
#'
setClass(
  Class = "ConstantInternalFluxRateList_by_PoolName",
  contains=c("list")
)

#' constructor from a normal list 
#' 
#' @param object A list. Either a list of elements of type  
#' \linkS4class{ConstantInternalFluxRate_by_PoolName} or 
#' a list where the names of the elements are strings of the form
#' 'somePool->someOtherPool' (for the flux rate from pool somePool to
#' someOtherPool)
#'
#' @return An object of class
#' \linkS4class{ConstantInternalFluxRateList_by_PoolName} 
#' 
#' 
#' The function checks if the elements are of the desired type or can be
#' converted to it. It is mainly used internally and usually called 
#' by the front end functions to convert the user supplied arguments.

setMethod("ConstantInternalFluxRateList_by_PoolName",
    signature=signature(object="list"),
    definition=function(object){
        makeListInstance(
            object
            ,targetClassName='ConstantInternalFluxRate_by_PoolName'
            ,targetListClassName="ConstantInternalFluxRateList_by_PoolName"
            ,permittedValueClassName='numeric'
            ,key_value_func=function(key,val){
                ConstantInternalFluxRate_by_PoolName(
                     src_to_dest=key
                    ,rate_constant=val
                )
            }
        )
    }
)


