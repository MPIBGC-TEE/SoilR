#' Subclass of list that is guaranteed to contain only elements of type
#' \linkS4class{ConstantOutFluxRate_by_PoolIndex}
#'
#' The main purpose of the class is to be used in method signatures 
#' which would otherwise only indicate an object of class \code{'list'}
#' in their signature an then check that the list contains the right
#' kind of elements inside the function.
#' Using this class the method signature becomes much more informative. 
setClass(
  Class = "ConstantOutFluxRateList_by_PoolIndex",
  contains=c("list")
)

#' constructor from a normal list 
#' 
#' @param object A list. Either a list of elements of type  
#' \linkS4class{ConstantOutFluxRate_by_PoolIndex} or 
#' a list where the names of the elements are integer strings of the form
#' '3' (for the flux rate from pool 3) 
#'
#' @return An object of class
#' \linkS4class{ConstantOutFluxRateList_by_PoolIndex} 
#' 
#' 
#' The function checks if the elements are of the desired type or can be
#' converted to it. It is mainly used internally and usually called 
#' by the front end functions to convert the user supplied arguments.

setMethod("ConstantOutFluxRateList_by_PoolIndex",
    signature=signature(object="list"),
    definition=function(object){
        targetClassName='ConstantOutFluxRate_by_PoolIndex'
        targetListClassName='ConstantOutFluxRateList_by_PoolIndex'
        
        if( allElementsAreOfClass(object,targetClassName)){
            return(as(object,targetListClassName))
        }else{
            if(allElementsAreOfClass(object,'numeric')){
                # try to convert the list elements
                # This is for something like
                # object=list('1'=0.3,'2'=0.4)
                keys=names(object)
                rates=lapply(
                    keys
                    ,function(key){
                        ConstantOutFluxRate_by_PoolIndex(
                            sourceIndex=PoolIndex(key)
                            ,rate_constant=object[[key]]
                        )
                    }
                )
                return(as(rates,targetListClassName))
            }else{
                stop(
                    paste(
                        'The list must contain either
                         instances of class '
                        ,targetClassName
                        , 'or must be  of the form list("1"=.4,...) where "1" is the source pool,
                        and 0.4 the rate constant. but I got the following list'
                        ,object
                    )
                )
            }
        }
    }
)

setMethod("ConstantOutFluxRateList_by_PoolIndex",
    signature=signature(object="numeric"),
    definition=function(object){
         # make it a list and call the constructor for list arguments
         ConstantOutFluxRateList_by_PoolIndex(as.list(object))
    }
)

#' convert to a list indexed by pool names
#'
setMethod("by_PoolName",
    signature=signature(obj="ConstantOutFluxRateList_by_PoolIndex"),
    definition=function(obj,poolNames){
        l=lapply(
                obj
                ,function(rate){
                    by_PoolName(rate,poolNames)
                }
        )
        as(l,'ConstantInternalFluxRateList_by_PoolName')
    }
)
