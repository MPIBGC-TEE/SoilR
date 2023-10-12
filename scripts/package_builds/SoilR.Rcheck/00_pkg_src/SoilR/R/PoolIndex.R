#' construct from number 
#' @param id pool id
setMethod(
    f="PoolIndex",
    signature=c(id='numeric'),
    def=function(id){
        check_id_length(id)
        return(new('PoolIndex',id))
    }
)

##' construct from number string like '1' or '3'
##'
##'@param id pool id
setMethod(
    f="PoolIndex",
    signature=c(id='character'),
    def=function(id){
        assertthat::assert_that(
            grepl('^[0-9]+$',id)
            ,msg=paste("Cant convert the id ",id,"to an integer")
        )
        check_id_length(id)
        return(new('PoolIndex',as.integer(id)))
    }
)

#' pass through constructor fron an object of the same class
#'
#' This is here to be able to call PoolIndex on a PoolIndex object without
#' having to check before if it is necessary.
#' the unnecessary poolNames argument will be ignored.
#' @param id pool id
#' @param poolNames names of pools
setMethod(
    f="PoolIndex",
    signature=c(id='PoolIndex'),
    def=function(id,poolNames){ id }
)

#' convert to string like object  
#' @param id pool id
#' @param poolNames name of pools
setMethod(
    f="PoolName",
    signature=c(id='PoolIndex'),
    def=function(id,poolNames){
        PoolName(poolNames[[id]]) 
    }
)
getRecipientIndex=function(src_to_dest){
  #PoolIndex(as.integer(unlist(strsplit(stri,split=fromToSplitter()))[[2]]))
  PoolIndex(as.integer(src_to_dest_parts(src_to_dest)[[2]]))
}
getSenderIndex=function(src_to_dest){
  #PoolIndex(as.integer(unlist(strsplit(stri,split=fromToSplitter()))[[1]]))
  PoolIndex(as.integer(src_to_dest_parts(src_to_dest)[[1]]))
}
