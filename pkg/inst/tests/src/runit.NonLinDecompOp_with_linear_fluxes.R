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
  BFunc=UnBoundNonLinDecompOp(
    internal_fluxes=intfs
    ,out_fluxes=ofs
    ,numberOfPools=3
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
