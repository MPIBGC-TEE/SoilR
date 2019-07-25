
#' class for pool-name-strings
setClass(
   Class="PoolName",
   ,contains=c('PoolId','character')
)

#' construct from string with checks
setMethod(
    f="PoolName",
    signature=c(id='character'),
    def=function(id){
        check_id_length(id)
        if(make.names(id)!=id){
           stop('By convention PoolIds in SoilR have to be valid R identifiers. You can check with the function make.names. If make.names("your_pool_name") == "your_pool_name" then the pool name is ok.')
        }
        return(new('PoolName',id))
    }
)
#' pass throug constructor fron an object of the same class
#'
#' This is here to be able to call PoolName on a PoolName ojbect without
#' having to test before if we have to. 
# This makes the calling code easier to read. 
setMethod(
    f="PoolName",
    signature=c(id='PoolName'),
    def=function(id,poolNames){id}
)

#' convert to number like object
setMethod(
    f="PoolIndex",
    signature=c(id='PoolName'),
    def=function(id,poolNames){ 
        if (missing(poolNames)){
            stop('Cant convert name to index if no vector with all pool names is provided.')
        }
        check_duplicate_pool_names(poolNames)
        res<-grep(id,poolNames)
        if (length(res)<1){
            stop(
              paste0(
                  c(
                    "The name:"
                    ," was not found in poolNames:"
                  )
                  ,
                  c(
                    id
                    ,poolNames
                  )
              )
            )
        }
        PoolIndex(res)
    }
)
getRecipientName=function(src_to_dest){
  PoolName(src_to_dest_parts(src_to_dest)[[2]])
}
getSenderName=function(src_to_dest){
  PoolName(src_to_dest_parts(src_to_dest)[[1]])
}
