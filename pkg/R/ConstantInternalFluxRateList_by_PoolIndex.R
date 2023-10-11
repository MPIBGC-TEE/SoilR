
#' @template FluxRate
#'
#' @autocomment 
#' Subclass of list that is guaranteed to contain only elements of type
#' \linkS4class{ConstantInternalFluxRate_by_PoolIndex}
#' @s4superclasses
#' @s4subclasses
#' @s4methods
setClass(
   Class="ConstantInFluxRate_by_PoolName",
   slots=c(destinationName='PoolName',rate_constant='numeric')
)

ConstantInFluxRate_by_PoolName<-function(destinationName,rate_constant){
    if (rate_constant<0){
      stop(
        "Negative rate constant. 
        A rate_constant defines a flux F with F = rate_constant*pool_content. 
        Since fluxes have to be positive and pool contents are positive
        rate constants have to be positive too."
      )
    }
    new(
        'ConstantInFluxRate_by_PoolName'
        ,destinationName=PoolName(id=destinationName)
        ,rate_constant=rate_constant
    )
}

#' @template FluxRateList
#'
#' @autocomment 
#' @s4superclasses
#' @s4subclasses
#' @s4methods
setClass(
   Class="ConstantInFluxRate_by_PoolName",
   slots=c(destinationName='PoolName',rate_constant='numeric')
)

ConstantInFluxRate_by_PoolName<-function(destinationName,rate_constant){
    if (rate_constant<0){
      stop(
        "Negative rate constant. 
        A rate_constant defines a flux F with F = rate_constant*pool_content. 
        Since fluxes have to be positive and pool contents are positive
        rate constants have to be positive too."
      )
    }
    new(
        'ConstantInFluxRate_by_PoolName'
        ,destinationName=PoolName(id=destinationName)
        ,rate_constant=rate_constant
    )
}

#' new object with the source pool id converted to a PoolIndex if necessary 
#' @param obj and object of class ConstantInFluxRate_by_PoolName
#' @param poolNames names of pools
setMethod(
  f="by_PoolIndex",
  signature=c(obj='ConstantInFluxRate_by_PoolName'),
  def=function(obj,poolNames){
      new(
        "ConstantInFluxRate_by_PoolIndex"
        ,destinationIndex=PoolIndex(id=obj@destinationName,poolNames)
        ,rate_constant=obj@rate_constant
      )
  }
)

# This method seems to be identical to the previous one. It's now commented out but eventually should be removed 
# setMethod(
#   f="by_PoolIndex",
#   signature=c(obj='ConstantInFluxRate_by_PoolName'),
#   def=function(obj,poolNames){
#       new(
#         "ConstantInFluxRate_by_PoolIndex"
#         ,destinationIndex=PoolIndex(id=obj@destinationName,poolNames)
#         ,rate_constant=obj@rate_constant
#       )
#   }
# )

#' @template FluxRateList
#'
#' @autocomment 
#' @s4superclasses
#' @s4subclasses
#' @s4methods
setClass(
   Class="ConstantInFluxRate_by_PoolName",
   slots=c(destinationName='PoolName',rate_constant='numeric')
)

ConstantInFluxRate_by_PoolName<-function(destinationName,rate_constant){
    if (rate_constant<0){
      stop(
        "Negative rate constant. 
        A rate_constant defines a flux F with F = rate_constant*pool_content. 
        Since fluxes have to be positive and pool contents are positive
        rate constants have to be positive too."
      )
    }
    new(
        'ConstantInFluxRate_by_PoolName'
        ,destinationName=PoolName(id=destinationName)
        ,rate_constant=rate_constant
    )
}

# #' new object with the source pool id converted to a PoolIndex if necessary 
# setMethod(
#   f="by_PoolIndex",
#   signature=c(obj='ConstantInFluxRate_by_PoolName'),
#   def=function(obj,poolNames){
#       new(
#         "ConstantInFluxRate_by_PoolIndex"
#         ,destinationIndex=PoolIndex(id=obj@destinationName,poolNames)
#         ,rate_constant=obj@rate_constant
#       )
#   }
# )

#' @template FluxRateList
#'
#' @autocomment 
#' @s4superclasses
#' @s4subclasses
#' @s4methods
setClass(
  Class = "ConstantInternalFluxRateList_by_PoolIndex",
  contains=c("list")
)

#' constructor from a normal list 
#' 
#' @param object A list. Either a list of elements of type  
#' \linkS4class{ConstantInternalFluxRate_by_PoolIndex} or 
#' a list where the names of the elements are strings of the form
#' '1->3' (for the flux rate from pool 1 to 2)
#'
#' @return An object of class
#' \linkS4class{ConstantInternalFluxRateList_by_PoolIndex} 
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
#' @param object no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
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
#' @param obj an object of class ConstantInternalFluxRateList_by_PoolIndex
#' @param poolNames names of pools
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

