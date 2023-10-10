# test the constructor
test.ConstLinDecompOp_check_external_flux_args=function(){
  mess='the out flux pool index:5 exceeds the numberOfPools:4'
  checkException(
    ConstLinDecompOp(
      internal_flux_rates=ConstantOutFluxRateList_by_PoolIndex(list("1_to_2"=3))
      ,out_flux_rates=ConstantOutFluxRateList_by_PoolIndex(list("5"=2))
      ,numberOfPools = 4
    )
    ,mess
    #,silent=TRUE
  )
}


test.ConstLinDecompOp_check_internal_flux_args=function(){
  mess='the internal flux pool index:5 exceeds the numberOfPools:4'
  checkException(
    ConstLinDecompOp(
      internal_flux_rates=ConstantOutFluxRateList_by_PoolIndex(list("5_to_2"=3))
      ,out_flux_rates=ConstantOutFluxRateList_by_PoolIndex(list("1"=2))
      ,numberOfPools = 4
    )
    ,silent=TRUE
  )
}


test.ConstLinDecompOpWithoutInternalFluxes=function(){
  n<-3
  k<-3
  out_flux_rates=ConstantOutFluxRateList_by_PoolIndex(list("1"=k))
  B=ConstLinDecompOp(
     out_flux_rates=out_flux_rates
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
    # we create an Operator that is equivalent to the
    # following Compartmental Matrix
    n<-3
    k<-3
    B_ref=matrix(
         nrow=n
        ,ncol=n
        ,byrow=TRUE
        ,c( 
           -k,0,0
           ,k,0,0
           ,0,0,0
        )
      )
    # we show different possible ways to supply the internal fluxes
    # and check the results against our reference


    #internally we use this form
    internal_flux_rates_1<-ConstantInternalFluxRateList_by_PoolIndex(
          c(
              ConstantInternalFluxRate_by_PoolIndex(
                  sourceIndex=1L
                  ,destinationIndex=2L
                  ,rate_constant=k
              )
          )
    )
    B_1<-ConstLinDecompOp(
         internal_flux_rates=internal_flux_rates_1
        ,numberOfPools= n
    )
    checkEquals(getConstantCompartmentalMatrix(B_1),B_ref)

}


test.ConstLinDecompOpFromNamedFluxes=function(){
    n<-3
    k<-3
    ifrs=ConstantInternalFluxRateList_by_PoolName(
        list(
            ConstantInternalFluxRate_by_PoolName(sourceName='barrel',destinationName='glass',rate_constant=k)
            )
    )
    ofrs=ConstantOutFluxRateList_by_PoolName(
        list(
            ConstantOutFluxRate_by_PoolName(sourceName='barrel',rate_constant=k)
         )
    )
    B=ConstLinDecompOp(
        internal_flux_rates=ifrs
        ,out_flux_rates=ofrs
        ,poolNames=c('barrel','glass','belly')
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
