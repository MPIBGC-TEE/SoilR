setClass(
   Class="PoolId",
   slots=c(ind='integer',name='character')
)
#' constructor
setMethod(
   f="PoolId",
   signature=c(id='character'),
   def=function(id){
       if(!length(id)==1){
           stop('If the index is given as character vector the length must be exactly 1.')}
       if(make.names(id)!=id){
          stop('By convention PoolIds in SoilR have to be valid R identifiers. You can check with the function make.names. If make.names("your_pool_name") == "your_pool_name" then the pool name is ok.')
       }
       return(new('PoolId',name=id))
   }
)
#' constructor
setMethod(
    f="PoolId",
    signature=c(id='numeric'),
    def=function(id){
        return( new('PoolId',ind=PoolIndex(id)))
   }
)

#'  
setMethod(
    f="PoolIndex",
    signature=c(object='PoolId'),
    def=function(object,stateVariableNames=NULL){
    if (length(object@ind)>0){
        res<-object@ind
    }else{
        dinds<-stateVariableNames[duplicated(stateVariableNames)]
        print('###############################################################################3333')
        print(dinds)
        if (length(dinds)>0){
            stop(
                 paste0('The following state variable names were duplicated:',dinds)
            )
        } 
        res<-grep(object@name,stateVariableNames)
        if (length(res)<1){
            stop(
              paste0(
                  c(
                    "name:"
                    ," non found in stateVariableNames:"
                  )
                  ,
                  c(
                    object@name
                    ,stateVariableNames
                  )
              )
            )
         }
    }
   res 
    }
 )

setMethod(
    f="PoolIndex",
    signature=c(object='numeric'),
    def=function(object){
        if (object !=as.integer(object)){stop('Pool indeces have to be (positve) integers')}
        if (object<1){stop('A numerical PoolId has to be >=1')}
        as.integer(object)
    }
)
setMethod(
    f="is.numeric",
    signature=c(x='PoolId'),
    def=function(x){
        length(x@ind)>0
    }
)   
