#' A non-autonomous version of the original Wang 3 pool model
#' 
#' An Example based on the original non-linear autonomous model as described in  
#' \url{http://doi.org/10.5194/bg-11-1817-2014}
#' with state_variables:
#' \enumerate{
#'  \item C_l
#'    desc: litter carbon
#'    unit: "g C m^{-2}"
#'  \item C_s
#'    desc: soil organic matter
#'    unit: "g C m^{-2}"
#'  \item C_b: 
#'    desc: microbial biomass
#'    unit: "g C m^{-2}"
#' }
#' Note that this is not a complete model run like most of the models in SoilR but a 
#' description of the fluxes that can be extended to a model run if initial values
#' and times are specified.
#' The default values are completely arbitrary.
#' So is one time dependency that has been added to demonstrate that this is possible everywhere and every 
#' part of the model can become non-autonomous. 
#' At the moment the variable t is mostly ignored like in the original Wang Model except for the first
#' influx to pool C_l.
#' 
#' @param alpha fraction of carbon influx that directly enters the soil organic matter pool
#' @param epsilon  microbial growth efficiency
#' @param mu_b turnover rate of microbial biomass per year, unit: "year^{-1}"
#' @param F_NPP  carbon influx into soil, unit: "g C*m^{-2}*year^{-1}"
#' @param V_l 
#' maximum rate of litter carbon assimilation per unit microbial biomass per year
#' @param V_s maximum rate of soil carbon assimilation per unit microbial biomass per year
#' @param K_l half-saturation constant for litter carbon assimilation by microbial biomass
#' @param K_s half-saturation constant for soil carbon assimilation by microbial biomass
#' @examples
#' # This is a working example which demostrates some of the new functionality.
#' require('SoilR',quietly =TRUE)
#' smod <- WangThreePoolNonAutonomous_sym() 
#' # (look at the source code of WangThreePoolNonAutonomous_sym )
#' plotPoolGraph(smod)
#' state_variable_names(smod)
#' # define initial values for the state variables
#' iv=c(C_l=1000,C_b=5000,C_s=1000)
#' times<-seq(from=1,to=1000,by=10)
#' modrun=Model_by_PoolNames( smod=smod ,times=times ,initialValues=iv)
#' sol <- getSolution(modrun)
#' # Let's see what we have computed
#' colnames(sol)
#' # shortcut overview plot for all phase plane projections and time lines
#' # of the pool contents
#'  plot(data.frame(times=times,sol[,c('C_l','C_s','C_b')]))
#' # plot fluxes as functions of time
#' 
#' in_fluxes <- sol[,grep('influxes',colnames(sol))]
#' plot( times, sol[,'influxes.C_l'] ,type='l'
#'   ,ylim=c(min(in_fluxes),max(in_fluxes))
#' )
#' lines( times, sol[,'influxes.C_l'] ,type='l'
#'   ,ylim=c(min(in_fluxes),max(in_fluxes))
#' )
#' internal_fluxes <- sol[,grep('internal_fluxes',colnames(sol))]
#' plot(
#'   times, sol[,'internal_fluxes.C_l->C_b'] ,type='l'
#'   ,ylim=c(min(internal_fluxes),max(internal_fluxes))
#' )
WangThreePoolNonAutonomous_sym<- function(
    alpha=0.5,
    epsilon=0.4,
    mu_b=.2,
    F_NPP=3000000,
    V_l=0.5,
    V_s=0.5,
    K_l=100000,
    K_s=100
  ){
  time_symbol='t'

  ifs=InFluxList_by_PoolName(
        c(
            InFlux_by_PoolName(
                destinationName='C_l'
                ,func=function(t){ 
                    F_NPP*(1-alpha)*(1+0.5*sin(t/200)) # time dependent influx
                }
            )
            ,InFlux_by_PoolName(
                destinationName='C_s'
                ,func=function(t){ 
                    F_NPP*alpha
                }
            )
        )
  )
  ofs=OutFluxList_by_PoolName(
        c(
            OutFlux_by_PoolName(
                sourceName='C_l'
                ,func=function(C_l,C_b){
                  C_b*C_l*V_l*(1-epsilon)/(C_l+K_l) 
                }
            )
            ,
            OutFlux_by_PoolName(
                sourceName='C_s'
                ,func=function(C_l,C_b,C_s){
                  C_b*C_s*V_s*(1-epsilon)/(C_s+K_s) 
                }
            )
        )
  )
  intfs=InternalFluxList_by_PoolName(
    list(
        InternalFlux_by_PoolName(
            sourceName='C_l'
            ,destinationName='C_b'
            ,func=function(C_b,C_l){
              C_b*C_l*V_l*epsilon/(C_l+K_l) 
            }
        )
        ,InternalFlux_by_PoolName(
            sourceName='C_s'
            ,destinationName='C_b'
            ,func=function(C_l,C_b,C_s){
              C_b*C_s*V_s*epsilon/(C_s+K_s) 
            }
        )
        ,InternalFlux_by_PoolName(
            sourceName='C_b'
            ,destinationName='C_s'
            ,func=function(C_b,t){
              C_b*mu_b

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
