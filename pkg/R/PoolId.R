#' helper function to check that the length of the argumetn is exactly 1
#'
#' @param id Either a string or a number 
check_id_length<-function(id){
    if(!length(id)==1){
        stop('The length of a pool must be exactly 1. (either a string of a number)')
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
#' the class returned depends on the method dispached depending on the supplied arguments 
setMethod(
    f="GeneralPoolId",
    signature=c(id='numeric'),
    def=function(id){
        return(PoolIndex(id))
   }
)

#' @auto
setMethod(
    f="GeneralPoolId",
    signature=c(id='character'),
    def=function(id){
        return(PoolName(id))
   }
)

