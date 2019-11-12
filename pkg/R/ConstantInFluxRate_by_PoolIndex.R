#' automatic title
#' 
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setClass(
   Class="ConstantInFluxRate_by_PoolIndex",
   slots=c(destinationIndex='PoolIndex',rate_constant='numeric')
)

ConstantInFluxRate_by_PoolIndex<-function(destinationIndex,rate_constant){
    if (rate_constant<0){
        stop(
            "Negative rate constant. 
            A rate_constant defines a flux F with F = rate_constant*pool_content. 
            Since fluxes have to be positive and pool contents are positive
            rate constants have to be positive too."
        )
    }
    new(
        'ConstantInFluxRate_by_PoolIndex'
        ,destinationIndex=PoolIndex(id=destinationIndex)
        ,rate_constant=rate_constant
    )
}

#' new object with the source pool id converted to a PoolIndex if necessary 
setMethod(
    f="by_PoolName",
    signature=c(obj='ConstantInFluxRate_by_PoolIndex'),
    def=function(obj,poolNames){
        new(
            "ConstantInFluxRate_by_PoolName"
            ,destinationName=PoolName(id=obj@destinationIndex ,poolNames)
            ,rate_constant=obj@rate_constant
        )
    }
)
