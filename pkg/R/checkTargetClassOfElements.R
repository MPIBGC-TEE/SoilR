checkTargetClassOfElements<-function(l,targetClassName){
    if(
        !all(
            as.logical(
                lapply(
                    l
                    ,function(el){inherits(el,targetClassName)}
                )
            )
        )
    ){
        stop(
            paste(
                'The list elements have to be of type:'
                ,targetClassName
            )
        )
    }
    as(l,'InternalFluxList_by_PoolIndex')

}
