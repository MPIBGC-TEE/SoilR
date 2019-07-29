# test the constructor
test.function_by_PoolIndex=function(){
    poolNames=c('barrel','glass','belly')
    timeSymbol='t'
    funcOfVars=function(belly,glass,barrel,t){ # we mix up the order
        # we return a vector for test reasons only
        # in the application the result will be a scalar
        # but it is easier to check that the result is correct this way
        c(belly,glass,barrel,t) 
    }
    funcOfVec=by_PoolIndex(funcOfVars,poolNames,timeSymbol)
    belly=1;glass=2;barrel=3;t=4
    rev=c(belly,glass,barrel,t) # same as funcOf Vars returns

    res_vars=funcOfVars(belly,glass,barrel,t)
    checkEquals(res_vars,rev)

    vec=c(barrel,glass,belly) # in order of the poolNames
    # now apply the two functions to equivalent arguments and check that the result is the same
    res_vec=funcOfVec(vec,t)
    checkEquals(res_vec,rev)

    # another example with a function that depends only on a 
    # single argument
    leaf_resp=function(leaf_pool_content){leaf_pool_content*4}
    leaf_resp(1)
    poolNames=c(
       'some_thing'
      ,'some_thing_else'
      ,'some_thing_altogther'
      ,'leaf_pool_content'
    )
    # create a version of the function f(vec,t) 
    leaf_resp_vec=by_PoolIndex(leaf_resp,poolNames,timeSymbol='t')
    # The result is the same since the only the forth position in the vector
    # is equal to our original leaf_pool_content=1
    leaf_resp_vec(c(1,27,3,1),5) 
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
    list(
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
    
  ofs=OutFluxList_by_PoolName(
        c(
            OutFlux_by_PoolName(
                sourceName='barrel'
                ,func=function(barrel,glass,t){
                    k*barrel 
                    # just a linear donor dependent 
                    # flux the second argument is fake but here for the test
                }
            )
        )
  )
  #ifs=OutFluxList_by_PoolName(
  #      c(
  #          InFlux_by_PoolName(
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
  BFunc=getCompartmentalMatrixFunc(
    obn
    ,timeSymbol
    ,state_variable_names
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
  #initial_values=list(barrel=0.4,glass=0,belly=0)
  #m=CorradosGeneralModel(operator=obn,influxes=ifs,times,initial_values,chi=chi_func)
}
