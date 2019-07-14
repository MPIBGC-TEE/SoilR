# test the constructor
test.ConstLinDecompOp_check_external_flux_args=function(){
  mess='the out flux pool index:5 exceeds the numberOfPools:4'
  checkException(
    ConstLinDecompOp(
      internal_flux_rates=c("1_to_2"=3)
      ,out_flux_rates=c("5"=2)
      ,numberOfPools = 4
    )
    ,mess
    ,silent=TRUE
  )
}
test.ConstLinDecompOp_check_internal_flux_args=function(){
  mess='the internal flux pool index:5 exceeds the numberOfPools:4'
  checkException(
    ConstLinDecompOp(
      internal_flux_rates=c("5_to_2"=3)
      ,out_flux_rates=c("1"=2)
      ,numberOfPools = 4
    )
    ,mess
    ,silent=TRUE
  )
}
test.ConstLinDecompOpWithoutInternalFluxes=function(){
  n<-3
  k<-3
  B=ConstLinDecompOp(
     out_flux_rates=c("1"=k)
    ,numberOfPools = n
  )@mat
  print(B)
  checkEquals(
     B
    ,matrix(
       nrow=n
      ,ncol=n
      ,byrow=TRUE
      ,c( 
         -3,0,0
         ,0,0,0
         ,0,0,0
      )
    )
  )
}
test.ConstLinDecompOpWithoutOutFluxes=function(){
  n<-3
  k<-3
  B=ConstLinDecompOp(
    internal_flux_rates=c("1_to_2"=k)
    ,numberOfPools = n
  )@mat
  print(B)
  checkEquals(
     B
    ,matrix(
       nrow=n
      ,ncol=n
      ,byrow=TRUE
      ,c( 
         -3,0,0
         ,3,0,0
         ,0,0,0
      )
    )
  )
}
test.ConstLinDecompOp=function(){
  n<-3
  k<-3
  B=ConstLinDecompOp(
    internal_flux_rates=c("1_to_2"=k)
    ,out_flux_rates=c("1"=k)
    ,numberOfPools = n
  )@mat
  print(B)
  checkEquals(
     B
    ,matrix(
       nrow=n
      ,ncol=n
      ,byrow=TRUE
      ,c( 
         -6,0,0
         ,3,0,0
         ,0,0,0
      )
    )
  )
}
test.ConstLinDecompOpFromNamedFluxes=function(){
  n<-3
  k<-3
  ifrs=c(
    ConstantInternalFluxRate(source='barrel',destination='glass',rate_constant=k)
  )
  ofrs=c(
    ConstantOutFluxRate(source='barrel',rate_constant=k)
  )
  B=ConstLinDecompOp(
    internal_flux_rates=c("1_to_2"=k)
    ,out_flux_rates=ofrs
    ,poolNames=c('barrel','glass','belly')
  )@mat
  #print(B)
  #checkEquals(
  #   B
  #  ,matrix(
  #     nrow=n
  #    ,ncol=n
  #    ,byrow=TRUE
  #    ,c( 
  #       -6,0,0
  #       ,3,0,0
  #       ,0,0,0
  #    )
  #  )
  #)
}
