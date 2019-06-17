setClass(
   Class="PoolIndex",
   contains="integer"
)
setMethod(
   f="PoolIndex",
   signature=c(number='numeric'),
   def=function(number){
       if (number!=as.interger(number)){stop('a PoolIndex has to be an integer')}
       if (number<1){'A PoolIndex has to be >=1'}
       return( new('PoolIndex',number))
   }
)
