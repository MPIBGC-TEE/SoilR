Wang3p <- function(iv,times){
 time_symbol='t'
 smod <- Wang3psym(time_symbol=time_symbol)
 intfs <- smod@internal_fluxes
 ifs <- smod@in_fluxes
 ofs <- smod@out_fluxes

 obn<- UnBoundNonLinDecompOp_by_PoolNames(
   internal_fluxes=intfs
   ,out_fluxes=ofs
   ,timeSymbol=time_symbol
 )
 # define initial values for the state variables
 #iv=c(barrel=1,bottle=2,glass=5)
  ivp <- IVP_maker( 
    in_fluxes=ifs,
	  internal_fluxes=intfs,
	  out_fluxes=ofs,
	  time_symbol='t',
	  startValues=iv
  )
  #print(ydot(1,startValues,ks))
  modrun=GeneralModel(
        t=times
        ,A=obn
        ,ivList=iv
        ,inputFluxes=ifs
        ,timeSymbol=time_symbol
  )
  modrun
}
