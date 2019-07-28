
#' Subclass of list that is guaranteed to contain only elements of type
#' \linkS4class{ConstantInternalFluxRate_by_PoolName}
#'
setClass(
  Class = "ConstantInternalFluxRateList_by_PoolName",
  contains=c("list")
)

#' constructor from a normal list 
#' 
#' @param l A list. Either a list of elements of type  
#' \linkS4class{ConstantInternalFluxRate} or 
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

#setMethod("ConstantInternalFluxRateList_by_PoolName",
#    signature=signature(object="list"),
#    definition=function(object){
#        #makeListInstance(object,targetClassName='ConstantInternalFluxRate_by_Name',targetListClassName='ConstantInternalFluxRateList_by_PoolIndex')
#        targetClassName='ConstantInternalFluxRate_by_Name'
#        if(allElementsAreOfClass(object,targetClassName)){
#            return(as(object,targetListClassName))
#        }else{ 
#            if(allElementsAreOfClass(object,'numeric')){
#                # try to convert the list elements
#                # This is for something like
#                # object=list('1->2=0.3,'2->3'=0.4)
#                keys=names(object)
#                l=lapply(
#                        keys
#                        ,function(key){
#                            eval(
#                            call(targetListClassName,
#                                src_to_dest=key,
#                                ,rate_constant=object[[key]]
#                            ))
#                        }
#                )
#                # and then coerce to our special list tpye
#                return(as(l,targetListClassName))
#            }else{
#                stop(
#                    paste(
#                        'The list must contain either
#                         instances of class '
#                        ,targetClassName
#                        , 'or must be  of the form list("a->b"=.4,...) where "a" is the source pool name , "b" the target pool name and 0.4 is the rate constant. but I got the following list'
#                        ,object
#                    )
#                )
#            }
#        }
#    }
#)

