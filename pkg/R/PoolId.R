#' helper function to check that the length of the argument is exactly 1
#'
#' @param id Either a string or a number 
check_id_length<-function(id){
    if(!length(id)==1){
        stop('The length of a pool-id must be exactly 1. (either a string  or a number)')
    }
}

#' helper function 
#'
#' Check that poolNames are unique
#' @param poolNames character vector which will be tested for duplicats
check_duplicate_pool_names<-function(poolNames){
    dinds<-poolNames[duplicated(poolNames)]
    if (length(dinds)>0){
        stop(
             paste0('The following poolNames were duplicated:',dinds)
        )
    } 
}

#' generic factory for this virtual class
#'
#' the class returned depends on the method dispatched depending on the supplied arguments 
#' @param id a numerical id
setMethod(
    f="GeneralPoolId",
    signature=c(id='numeric'),
    def=function(id){
        return(PoolIndex(id))
   }
)

#' automatic title
#' 
#' @param id no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
    f="GeneralPoolId",
    signature=c(id='character'),
    def=function(id){
        return(PoolName(id))
   }
)

