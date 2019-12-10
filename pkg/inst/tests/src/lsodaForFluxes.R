#!/usr/bin/Rscript
requireNamespace('deSolve')
requireNamespace('RUnit')
requireNamespace("pkgload")
requireNamespace("purrr")
pkgload::load_all('../../../')


#------------------------------------------------------
#test.ydotCreation=function(){
  ks<-c(k_barrel=0.1,k_bottle=0.2)
  k=0.2
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
                ,func=function(barrel,t){
                    k*barrel 
                    # just a linear donor dependent 
                    # flux the second argument is fake but here for the test
                }
            )
        )
  )
  ifs=InFluxList_by_PoolName(
        c(
            InFlux_by_PoolName(
                destinationName='barrel'
                ,func=function(barrel,glass,t){
                    # ignore arguments
                    #res <- (barrel+glass)*(1+sin(t))/100
                    res <- 1
                    res
                }
            )
        )
  )
 
 obn<- UnBoundNonLinDecompOp_by_PoolNames(
   internal_fluxes=intfs
   ,out_fluxes=ofs
   ,timeSymbol='t'
 )
 # define initial values for the state variables
 iv=c(barrel=1,bottle=2,glass=5)
  ivp <- IVP_maker( 
    in_fluxes=ifs,
	  internal_fluxes=intfs,
	  out_fluxes=ofs,
	  time_symbol='t',
	  startValues=iv
  )
  #print(ydot(1,startValues,ks))
  times<-1:100
  mod=GeneralModel(
        t=times
        ,A=obn
        ,ivList=iv
        ,inputFluxes=ifs
        ,timeSymbol='t'
  )
   
  dir_sol <- deSolve::lsoda(y=ivp$startValues,times=times,func=ivp$ydot,parms=ks)
  #print(dir_sol)
  print(getSolution(mod))
#}

#------------------------------------------------------
#test.ydotCreation()

