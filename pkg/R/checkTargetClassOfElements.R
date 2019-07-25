checkTargetClassOfElements<-function(l,targetClassName){
    if(allElementsAreOfClass(l,targetClassName)){
        stop(
            paste(
                'The list elements have to be of type:'
                ,targetClassName
            )
        )
    }
    as(l,targetClassName)

}
allElementsAreOfClass<-function(l,targetClassName){
        !all(
            as.logical(
                lapply(
                    l
                    ,function(el){is(el,targetClassName)}
                )
            )
        )
}
