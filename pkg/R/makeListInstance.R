makeListInstance<-function(
    object
    ,targetClassName
    ,targetListClassName
    ,permittedValueClassName
    ,key_value_func
){
    if(allElementsAreOfClass(object,targetClassName)){
        return(as(object,targetListClassName))
    
    }else{ 
        if(allElementsAreOfClass(object,permittedValueClassName)){
            keys=names(object)
            l=lapply(
                keys
                ,function(key){
                    value=object[[key]]
                    key_value_func(key,value)
                }
            )
            # and then coerce to our special list tpye
            return(as(l,targetListClassName))
        }else{
            stop(
                paste(
                    'The list must contain either
                     instances of class '
                    ,targetClassName
                    ,'or must have values of class:',
                    ,permittedValueClassName
                    ,"but I got:"
                    ,object
                )
            )
        }
    }
}
