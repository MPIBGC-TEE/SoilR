# test the constructor
test.ConstLinDecompOp_check_args=function(){
  mess='the out flux pool index:5 exceeds the numberOfPools:4'
  checkException(
    ConstLinDecompOp(
      internal_flux_rates=c("1_to_2"=3)
      ,out_flux_rates=c("5",2)
      ,numberOfPools = 4
    )
    ,mess
    ,silent=TRUE
  )
}
test.ConstLinDecompOp=function(){
  n=2
  checkEquals(
    ConstLinDecompOp(
      internal_flux_rates=c("1_to_2"=3)
      ,out_flux_rates=c("1",2)
      ,numberOfPools = n
    )@mat
    ,matrix(nrow=n,ncol=n,byrow=TRUE,c(-1,0,3,-1))
  )
}
