#' class for pool indices 
#'
#' used to dispatch pool index specific methods like conversion to names.
setClass(
   Class="PoolIndex",
   ,contains=c('PoolId','integer')
)

#' construct from number 
setMethod(
    f="PoolIndex",
    signature=c(id='numeric'),
    def=function(id){
        check_id_length(id)
        return(new('PoolIndex',id))
    }
)

#' pass throug constructor fron an object of the same class
#'
#' This is here to be able to call PoolIndex on a PoolIndex ojbect without
#' having to chech before if it is necessary.
#' the unnecessary poolNames argument will be ignored
setMethod(
    f="PoolIndex",
    signature=c(id='PoolIndex'),
    def=function(id,poolNames){ id }
)

#' convert to string like object  
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
