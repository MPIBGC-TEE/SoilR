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
