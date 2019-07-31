checkTargetClassOfElements<-function(l,targetClassName){
    if(!allElementsAreOfClass(l,targetClassName)){
        stop(
            paste(
                'The list elements have to be of type:'
                ,targetClassName
            )
        )
    }
}
allElementsAreOfClass<-function(object,targetClassName){
        result=all(as.logical(lapply(object,function(el){
                   res<-is(el,targetClassName)
                   res
        })))
        result
}
