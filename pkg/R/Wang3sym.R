Wang3psym <- function(time_symbol='t'){
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
                    res <- (barrel+glass)*(1+sin(t))/100
                    #res <- 1
                    res
                }
            )
        )
  )
 
 smod <- SymbolicModel_by_PoolNames(
    in_fluxes=ifs                                
   ,internal_fluxes=intfs
   ,out_fluxes=ofs
   ,timeSymbol=time_symbol
 )
 smod
}
