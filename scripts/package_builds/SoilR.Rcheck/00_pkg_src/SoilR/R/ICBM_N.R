#' Implementation of the ICBM/N model
#' 
#' This implementations follows the description in Katterer and Andren (2001, Eco Mod 136:191).  
#' 
#' @param i carbon input to the soil from plant production
#' @param k_Y decomposition rate of young pool Y
#' @param k_O decomposition rate of old pool O
#' @param r_e external effects on decomposition rates
#' @param e_Y yield efficiency of the soil organism community
#' @param h humification coefficient. Fraction of outflux from Y that is not respired and enters O
#' @param q_i C:N ratio of plant inputs
#' @param q_b C:N ratio of soil organism biomass
#' @export
# 
ICBM_N<- function(
    i=0.47,
    k_Y=0.259,
    k_O=0.0154,
    r_e=1,
    e_Y=0.362,
    h=0.243,
    q_i=18.8,
    q_b=5.0
  ){
  time_symbol='t'

  ifs=InFluxList_by_PoolName(
        list(
            InFlux_by_PoolName(
                destinationName='Y'
                ,func=function(t){ 
                    i
                }
            )
            ,InFlux_by_PoolName(
                destinationName='Y_N'
                ,func=function(t){ 
                    i/q_i
                }
            )
        )
  )
  ofs=OutFluxList_by_PoolName(
        list(
            OutFlux_by_PoolName(
                sourceName='Y'
                ,func=function(Y){
                  k_Y*r_e*(1-h)*Y
                }
            )
            ,
            OutFlux_by_PoolName(
                sourceName='Y_N'
                ,func=function(Y_N, Y){
                  k_Y*r_e*((1-h)/(1-e_Y))*(Y_N-(e_Y*Y/q_b))
                }
            )
            ,
            OutFlux_by_PoolName(
              sourceName='O'
              ,func=function(O){
                k_O*r_e*O
              }
            )
            ,
            OutFlux_by_PoolName(
              sourceName='O_N'
              ,func=function(O_N){
                k_O*r_e*O_N
              }
            )
        )
  )
  intfs=InternalFluxList_by_PoolName(
    list(
        InternalFlux_by_PoolName(
            sourceName='Y'
            ,destinationName='O'
            ,func=function(Y){
              k_Y*r_e*h*Y
            }
        )
        ,InternalFlux_by_PoolName(
            sourceName='Y_N'
            ,destinationName='O_N'
            ,func=function(Y){
              k_Y*r_e*h*(Y/q_b)
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
