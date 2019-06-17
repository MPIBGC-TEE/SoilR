setClass(
   Class="PoolIndex",
   contains="integer"
)
setMethod(
   f="PoolIndex",
   signature=c(i='numeric'),
   def=function(i){
       if (i!=as.integer(i)){stop('a PoolIndex has to be an integer')}
       if (i<1){stop('A PoolIndex has to be >=1')}
       return( new('PoolIndex',i))
   }
)
