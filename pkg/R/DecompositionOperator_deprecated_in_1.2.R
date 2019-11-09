


#' automatic title
#' 
#' @param .Object : no manual documentation
#' @param starttime : no manual documentation
#' @param endtime : no manual documentation
#' @param map : no manual documentation
#' @param lag : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
    f="initialize",
    signature="DecompositionOperator",
    definition=function 
    (.Object,
    starttime=numeric(),
    endtime=numeric(),
    map=function(t){t},
    lag=0
    ){
    warning("The class DecompositionOperator is deprecated. The fuctionallity is now implemented by class BoundLinDecompOp. To get rid of this warning change your code from:\n
    new(DecompositionOperator,other,args,...) to \n
    BoundLinDecompOp(other,args,...) ")
    .Object@starttime=starttime
    .Object@endtime=endtime
    .Object@map=map
    return(.Object)
    }
)



#' automatic title
#' 
#' @param object : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
    f="getTimeRange",
    signature="DecompositionOperator",
    definition=function 
    (object 
    ){
        return( c("t_min"=object@starttime,"t_max"=object@endtime))
    }
)



#' automatic title
#' 
#' @param object : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
    f="getFunctionDefinition",
    signature="DecompositionOperator",
    definition=function(object){
        return(object@map)
    }
)
