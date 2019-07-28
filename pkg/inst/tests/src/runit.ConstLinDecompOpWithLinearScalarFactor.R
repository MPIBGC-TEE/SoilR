## test the constructor
#test.ConstLinDecompOpWithLinearScalarFactor_check_external_flux_args=function(){
#  mess='the out flux pool index:5 exceeds the numberOfPools:4'
#  checkException(
#    ConstLinDecompOpWithLinearScalarFactor(
#      internal_flux_rates=c("1_to_2"=3)
#      ,out_flux_rates=c("5"=2)
#      ,numberOfPools = 4
#    )
#    ,mess
#    ,silent=TRUE
#  )
#}


#test.ConstLinDecompOpWithLinearScalarFactor_check_internal_flux_args=function(){
#  mess='the internal flux pool index:5 exceeds the numberOfPools:4'
#  checkException(
#    ConstLinDecompOpWithLinearScalarFactor(
#      internal_flux_rates=c("5_to_2"=3)
#      ,out_flux_rates=c("1"=2)
#      ,numberOfPools = 4
#    )
#    ,mess
#    ,silent=TRUE
#  )
#}
#
#
#test.ConstLinDecompOpWithLinearScalarFactorWithoutInternalFluxes=function(){
#  n<-3
#  k<-3
#  B=getConstantCompartmentalMatrix(
#        ConstLinDecompOpWithLinearScalarFactor(
#            out_flux_rates=c("1"=k)
#            ,numberOfPools = n
#            ,xi=function(t){sin(t)+2}
#        )
#  )
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
#test.ConstLinDecompOpWithLinearScalarFactorWithoutOutFluxes=function(){
#  n<-3
#  k<-3
#  B=ConstLinDecompOpWithLinearScalarFactor(
#    internal_flux_rates=c("1_to_2"=k)
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
#         ,3,0,0
#         ,0,0,0
#      )
#    )
#  )
#}
#
#
#test.ConstLinDecompOpWithLinearScalarFactor=function(){
#  n<-3
#  k<-3
#  B=ConstLinDecompOpWithLinearScalarFactor(
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
#test.ConstLinDecompOpWithLinearScalarFactorFromNamedFluxes=function(){
#  n<-3
#  k<-3
#  ifrs=c(
#    ConstantInternalFluxRate(source='barrel',destination='glass',rate_constant=k)
#  )
#  ofrs=c(
#    ConstantOutFluxRate(source='barrel',rate_constant=k)
#  )
#  B=ConstLinDecompOpWithLinearScalarFactor(
#    internal_flux_rates=ifrs
#    ,out_flux_rates=ofrs
#    ,poolNames=c('barrel','glass','belly')
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
