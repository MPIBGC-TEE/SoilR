#' helper function 
check_id_length<-function(id){
    if(!length(id)==1){
        stop('The length of a pool must be exactly 1. (either a string of a number)')
    }
}

#' helper function 
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

#' @auto

#' @auto
setMethod(
    f="GeneralPoolId",
    signature=c(id='character'),
    def=function(id){
        return(PoolName(id))
   }
)

