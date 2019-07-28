## test the constructor
#test.ConstLinDecompOp_check_external_flux_args=function(){
#  mess='the out flux pool index:5 exceeds the numberOfPools:4'
#  checkException(
#    ConstLinDecompOp(
#      internal_flux_rates=c("1_to_2"=3)
#      ,out_flux_rates=c("5"=2)
#      ,numberOfPools = 4
#    )
#    ,mess
#    #,silent=TRUE
#  )
#}
#
#
#test.ConstLinDecompOp_check_internal_flux_args=function(){
#  mess='the internal flux pool index:5 exceeds the numberOfPools:4'
#  checkException(
#    ConstLinDecompOp(
#      internal_flux_rates=c("5_to_2"=3)
#      ,out_flux_rates=c("1"=2)
#      ,numberOfPools = 4
#    )
#    ,silent=TRUE
#  )
#}
#
#
#test.ConstLinDecompOpWithoutInternalFluxes=function(){
#  n<-3
#  k<-3
#  out_flux_rates=list("1"=k)
#  B=ConstLinDecompOp(
#     out_flux_rates=out_flux_rates
#    ,numberOfPools = n
#  )@mat
#  print(B)
#  checkEquals(
#     B
#    ,matrix(
#       nrow=n
#      ,ncol=n
#      ,byrow=TRUE
#      ,c( 
#         -3,0,0
#         ,0,0,0
#         ,0,0,0
#      )
#    )
#  )
#}
#
#
#test.ConstLinDecompOpWithoutOutFluxes=function(){
#    # we create an Operator that is equivalent to the
#    # following Compartmental Matrix
#    n<-3
#    k<-3
#    B_ref=matrix(
#         nrow=n
#        ,ncol=n
#        ,byrow=TRUE
#        ,c( 
#           -k,0,0
#           ,k,0,0
#           ,0,0,0
#        )
#      )
#    # we show different possible ways to supply the internal fluxes
#    # and check the results against our reference
#
#
#    #internally we use this form
#    internal_flux_rates_1<-ConstantInternalFluxRateList_by_PoolIndex(
#          c(
#              ConstantInternalFluxRate_by_PoolIndex(
#                  sourceIndex=1L
#                  ,destinationIndex=2L
#                  ,rate_constant=k
#              )
#          )
#    )
#    B_1<-ConstLinDecompOp(
#         internal_flux_rates=internal_flux_rates_1
#        ,numberOfPools= n
#    )
#    checkEquals(getConstantCompartmentalMatrix(B_1),B_ref)
#
#    # but the constructor will also accept normal lists (vectors)
#    # if the elements have the right type
#    internal_flux_rates_2<-c(
#         ConstantInternalFluxRate_by_PoolIndex(
#            sourceIndex=1L
#            ,destinationIndex=2L
#            ,rate_constant=k
#        )
#    )
#    B_2<-ConstLinDecompOp(
#        internal_flux_rates=internal_flux_rates_2
#        ,numberOfPools= n
#    )
#    checkEquals(getConstantCompartmentalMatrix(B_2),B_ref)
#
#    # or even a list of rates if the names are characters
#    # that can be used to infer source and target pools
#    internal_flux_rates_3=list("1_to_2"=k)
#    B_3<-ConstLinDecompOp(
#        internal_flux_rates=internal_flux_rates_3
#        ,numberOfPools= n
#    )
#    checkEquals( getConstantCompartmentalMatrix(B_3) ,B_ref)
#
#    internal_flux_rates_4=list("1->2"=k)
#    B_4<-ConstLinDecompOp(
#         internal_flux_rates=internal_flux_rates_4
#        ,numberOfPools= n
#    )
#    checkEquals(getConstantCompartmentalMatrix(B_4),B_ref)
#}
#
#
#test.ConstLinDecompOp=function(){
#  n<-3
#  k<-3
#  B=ConstLinDecompOp(
#    internal_flux_rates=c("1_to_2"=k)
#    ,out_flux_rates=c("1"=k)
#    ,numberOfPools = n
#  )@mat
#  print(B)
#  checkEquals(
#     B
#    ,matrix(
#       nrow=n
#      ,ncol=n
#      ,byrow=TRUE
#      ,c( 
#         -6,0,0
#         ,3,0,0
#         ,0,0,0
#      )
#    )
#  )
#}
#
test.ConstLinDecompOpFromNamedFluxes=function(){
  n<-3
  k<-3
  ifrs=c(
    ConstantInternalFluxRate_by_PoolName(sourceName='barrel',destinationName='glass',rate_constant=k)
  )
  ofrs=c(
    ConstantOutFluxRate_by_PoolName(sourceName='barrel',rate_constant=k)
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
