#' Subclass of list that is guaranteed to contain only elements of type
#' \linkS4class{ConstantInternalFluxRate_by_PoolIndex}
#'
setClass(
  Class = "ConstantInternalFluxRateList_by_PoolIndex",
  contains=c("list")
)

#' constructor from a normal list 
#' 
#' @param object A list. Either a list of elements of type  
#' \linkS4class{ConstantInternalFluxRate_by_PoolIndex} or 
#' a list where the names of the elements are strings of the form
#' '1->3' (for the flux rate from pool 1 to 2
#'
#' @return An object of class
#' \linkS4class{ConstantInternalFluxRateList_by_PoolIndex} 
#' 
#' 
#' The function checks if the elements are of the desired type or can be
#' converted to it. It is mainly used internally and usually called 
#' by the front end functions to convert the user supplied arguments.

setMethod("ConstantInternalFluxRateList_by_PoolIndex",
    signature=signature(object="list"),
    definition=function(object){
        targetClassName='ConstantInternalFluxRate_by_PoolIndex'
        if(allElementsAreOfClass(object,targetClassName)){
            return(as(object,"ConstantInternalFluxRateList_by_PoolIndex"))
        }else{ 
            if(allElementsAreOfClass(object,'numeric')){
                # try to convert the list elements
                # This is for something like
                # object=list('1->2=0.3,'2->3'=0.4)
                keys=names(object)
                l=lapply(
                        keys
                        ,function(key){
                            ConstantInternalFluxRate_by_PoolIndex(
                                src_to_dest=key,
                                ,rate_constant=object[[key]]
                            )
                        }
                )
                # and then coerce to our special list tpye
                return(as(l,'ConstantInternalFluxRateList_by_PoolIndex'))
            }else{
                stop(
                    paste(
                        'The list must contain either
                         instances of class '
                        ,targetClassName
                        , 'or must be  of the form list("1->2"=.4,...) where "1" is the source pool, "2" the target pool and 0.4 the rate constant. but I got the following list'
                        ,object
                    )
                )
            }
        }
    }
)




#' automatic title
#' 
#' @autocomment These comments were created by the auto_comment_roclet by
#' @param object : : no manual documentation
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod("ConstantInternalFluxRateList_by_PoolIndex",
    signature=signature(object="numeric"),
    definition=function(object){
        keys=names(object)
        # first creat a list
        l=lapply(
                keys
                ,function(key){
                    ConstantInternalFluxRate_by_PoolIndex(
                        src_to_dest=key,
                        ,rate_constant=object[[key]]
                    )
                }
        )
        # and then coerce to our special list tpye
        as(l,'ConstantInternalFluxRateList_by_PoolIndex')
    }
)

#' convert to a list indexed by pool names
#'
setMethod("by_PoolName",
    signature=signature(obj="ConstantInternalFluxRateList_by_PoolIndex"),
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

