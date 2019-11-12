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
  Class = "InFluxList_by_PoolIndex",
  contains=c("list")
)

#' constructor from a normal list
#'
#' after checking the elememts
setMethod(
    "InFluxList_by_PoolIndex"
    ,signature=signature(object="list")
    ,definition=function(object){
        checkTargetClassOfElements(object,targetClassName='InFlux_by_PoolIndex')
        as(object,'InFluxList_by_PoolIndex')
    }
)



#' automatic title
#' 
#' @autocomment These comments were created by the auto_comment_roclet by
#' @param object : : no manual documentation
#' @param numberOfPools : : no manual documentation
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod("getFunctionDefinition",
    signature=signature(object="InFluxList_by_PoolIndex"),
    definition=function(
        object
        ,numberOfPools
    ){

        np=PoolIndex(numberOfPools)
        IvecFunc=function(X,t){
            Ivec=matrix(nrow=np,ncol=1,0)
            for (ifl in object){
                dest<-ifl@destinationIndex
                if (dest> numberOfPools){stop("The index of the destination pool must be smaller than the number of pools")}
                Ivec[dest,1]<-ifl@func(X,t)
            }
            return(Ivec)
        }
        iv=StateDependentInFluxVector(
            map=IvecFunc
            ,starttime=-Inf
            ,endtime= Inf
        )
        getFunctionDefinition(iv)
    }
)
