# test the constructor
test.NonLinDecompOp_with_linear_fluxes_by_Names=function(){
  n<-3
  k<-3
  intfs=c(
    InternalFlux(
         source='barrel'
        ,destination='glass'
        ,map=function(barrel,glass,t){
            k*barrel # just a linear donor dependent flux 
        }
    )
  )
  ofs=c(
    OutFlux(
         source='barrel'
        ,map=function(barrel){
            k*barrel # just a linear donor dependent flux 
        }
    )
  )
  BFunc=UnBoundNonLinDecompOp(
    internal_fluxes=intfs
    ,out_fluxes=ofs
    ,poolNames=c('barrel','glass','belly')
    ,timeSymbol='t'
  )@matFunc
  # initial values
  iv<-c(barrel=1,glass=1,belly=1)
  B<-BFunc(iv,t=0)
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
test.NonLinDecompOp_with_linear_fluxes_by_Index=function(){
  #     barrel
  #  X= glass
  #     belly
  n<-3
  k<-3
  intfs=c(
    InternalFlux_by_PoolIndex(
         sourceIndex=1
        ,destinationIndex=2
        ,func=function(X,t){
            barrel=X[[1]]
            k*barrel # just a linear donor dependent flux although we could use the glass argument 
        }
    )
  )
  ofs=c(
    OutFlux_by_PoolIndex(
         sourceIndex=1
        ,func=function(X,t){
            barrel=X[[1]]
            k*barrel # just a linear donor dependent flux 
        }
    )
  )
  #BFunc=UnBoundNonLinDecompOp(
  #  chi_func
  #  ,normalized_internal_fluxes=intfs
  #  ,normalized_out_fluxes=ofs
  #  ,numberOfPools=3
  #  ,timeSymbol='t'
  #)@matFunc
  BFunc=getCompartmentalMatrixFunc(
      UnBoundNonLinDecompOp(
      internal_fluxes=intfs
      ,out_fluxes=ofs
      ,numberOfPools=3
      ,timeSymbol='t'
    )
  )
  # initial values
  iv<-c(barrel=1,glass=1,belly=1)
  B<-BFunc(iv,t=0)
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
test.NonLinDecompOp_with_linear_fluxes_by_Name=function(){
  #     barrel
  #  X= glass
  #     belly
  state_variable_names=c("barrel" ,"glass" ,"belly")
  timeSymbol='t'
  n<-3
  k<-3
  intfs=InternalFluxList_by_PoolName(
    c(
        InternalFlux_by_PoolName(
            sourceName='barrel'
            ,destinationName='glass'
            ,func=function(barrel,t){
                # just a linear donor dependent flux 
                # although we could use the glass argument 
                k*barrel 
            }
        )
    )
  )
    
  #ofs=OutFluxList_by_PoolName(
  #      c(
  #          OutFlux_by_PoolName(
  #              sourceName='barrel'
  #              ,func=function(barrel,glass,t){
  #                  k*barrel 
  #                  # just a linear donor dependent 
  #                  # flux the second argument is fake but here for the test
  #              }
  #          )
  #      )
  #)
  #BFunc=UnBoundNonLinDecompOp(
  #  chi_func
  #  ,normalized_internal_fluxes=intfs
  #  ,normalized_out_fluxes=ofs
  #  ,numberOfPools=3
  #  ,timeSymbol='t'
  #)@matFunc
  obn<- UnBoundNonLinDecompOp_by_PoolNames(
    internal_fluxes=intfs
    ,out_fluxes=ofs
    ,timeSymbol='t'
  )
  BFunc=getCompartmentalMatrixFunc(obn)
#  # initial values
#  iv<-c(barrel=1,glass=1,belly=1)
#  B<-BFunc(iv,t=0)
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
}
