#' Implementation of a radiocarbon version of the Century model
#' 
#' This function implements a radiocarbon version of the Century model as described in Parton et al.
#' (1987).
#' 
#' @param t A vector containing the points in time where the solution is
#' sought.
#' @param ks A vector of length 7 containing the values of the decomposition
#' rates for the different pools. Units in per year.
#' @param C0 A vector of length 7 containing the initial amount of carbon for
#' the 7 pools.
#' @param F0_Delta14C A vector of length 7 containing the initial fraction of
#' radiocarbon for the 7 pools in Delta14C format.
#' @param surfaceIn A scalar or data.frame object specifying the amount of aboveground litter
#' inputs to the soil surface by time (mass per area per year).
#' @param soilIn A scalar or data.frame object specifying the amount of belowground litter
#' inputs to the soil by time (mass per area per year).
#' @param LN A scalar representing the lignin to nitrogen ratio of the plant
#' residue inputs.
#' @param Ls A scalar representing the fraction of structural material that is
#' lignin.
#' @param clay Proportion of clay in mineral soil.
#' @param silt Proportion of silt in mineral soil.
#' @param xi A scalar, data.frame, function or anything that can be converted
#' to a scalar function of time \code{\linkS4class{ScalarTimeMap}}  object
#' specifying the external (environmental and/or edaphic) effects on
#' decomposition rates.
#' @param xi_lag A time shift/delay  for the automatically 
#' created time dependent function xi(t) 
#' @param inputFc A Data Frame object containing values of atmospheric Delta14C
#' per time. First column must be time values, second column must be Delta14C
#' values in per mil.
#' @param lag A time shift/delay for the radiocarbon inputs
#' @param lambda Radioactive decay constant. By default lambda=-0.0001209681
#' y^-1 . This has the side effect that all your time related data are treated
#' as if the time unit was year.
#' @param solver A function that solves the system of ODEs. This can be
#' \code{\link{euler}} or \code{\link{deSolve.lsoda.wrapper}} or any other user
#' provided function with the same interface.
#' @return A Model Object that can be further queried
#' @template FluxBased-common
#' @seealso \code{\link{RothCModel}}. There are other
#' \code{\link{predefinedModels}} and also more general functions like
#' \code{\link{Model}}.
#' @references Parton, W.J, D.S. Schimel, C.V. Cole, and D.S. Ojima. 1987.
#' Analysis of factors controlling soil organic matter levels in Great Plain
#' grasslands. Soil Science Society of America Journal 51: 1173--1179. Sierra,
#' C.A., M. Mueller, S.E. Trumbore. 2012. Models of soil organic matter
#' decomposition: the SoilR package version 1.0. Geoscientific Model
#' Development 5, 1045-1060.
#' @examples
#' cal_yrs=seq(1900,2015, by=1/12)
#' APPT=50 # Assume 50 cm annual precipitation
#' Pmax=-40+7.7*APPT # Max aboveground production
#' Rmax=100+7.0*APPT # Max belowground production
#' abvgIn=52*Pmax/(Pmax+Rmax)
#' blgIn=52*Rmax/(Pmax+Rmax)
#' AtmC14=Graven2017[,c("Year.AD", "NH")]
#' 
#' cm=CenturyModel14(t=cal_yrs, surfaceIn = abvgIn, soilIn = blgIn, 
#'                   F0_Delta14C=rep(0,7), inputFc=AtmC14, LN=0.5, Ls=0.1)
#' C14t=getF14(cm)
#' 
#' poolNames=c("Surface structural", "Surface metabolic", "Belowground structural",
#'                "Belowground metabolic", "Active SOM", "Slow SOM", "Passive SOM")
#' plot(AtmC14, type="l", ylab="Delta 14C (per mil)")
#' matlines(cal_yrs,C14t, lty=1, col=2:8)
#' legend("topleft", poolNames, lty=1, col=2:8, bty="n")

CenturyModel14<- function 
  (t,      
   ks=52*c(STR.surface=0.076, MET.surface=0.28, STR.belowgroun=0.094,
     MET.belowground=0.35,ACT=0.14,SLW=0.0038,PAS=0.00013),
   C0=rep(0, 7),	
   surfaceIn,    
   soilIn,    
   F0_Delta14C,
   LN, 
   Ls, 
   clay=0.2, 
   silt=0.45, 
   xi=1,
   inputFc,
   lag=0,
   lambda=-0.0001209681,  
   xi_lag=0,
   solver=deSolve.lsoda.wrapper  
  )	
  { 
    t_start=min(t)
    t_end=max(t)
    if(length(ks)!=7) stop("ks must be of length = 7")
    if(length(C0)!=7) stop("the vector with initial conditions must be of length = 7")
    Fm=0.85-0.018*LN
    Fs=1-Fm

    if(class(surfaceIn)=='numeric' && class(soilIn)=='numeric') {
      if(length(surfaceIn)==1 && length(soilIn)==1){
        inputFluxes=BoundInFluxes(
                                  function(t){
                                   matrix(
                                    nrow=7,ncol=1, c(surfaceIn*Fs,surfaceIn*Fm,soilIn*Fs,soilIn*Fm,0,0,0))
                                  }, t_start, t_end
                    )
      }
    }
    if(class(surfaceIn)=="data.frame" && class(soilIn)=='data.frame'){
       surface_influx_func=splinefun(x=surfaceIn[,1], y=surfaceIn[,2])
       soil_influx_func=splinefun(x=soilIn[,1], y=soilIn[,2])
       inputFluxes= BoundInFluxes(map=function(t){matrix(nrow=7,ncol=1,c(Fs*surface_influx_func(t),
                                      Fm*surface_influx_func(t),Fs*soil_influx_func(t),
                                      Fm*soil_influx_func(t),0,0,0))}, t_start, t_end)
    }
#    if(class(surfaceIn)=='numeric' && class(soilIn)=='numeric') {
#      if(length(surfaceIn)==1 && length(soilIn)==1){
#        inputFluxes=ConstantInFluxList_by_PoolIndex(
#            list(
#                ConstantInFlux_by_PoolIndex(
#                    destinationIndex=1
#                    ,flux_constant=surfaceIn*Fs
#                ),
#                ConstantInFlux_by_PoolIndex(
#                    destinationIndex=2
#                    ,flux_constant=surfaceIn*Fm
#                ),
#                ConstantInFlux_by_PoolIndex(
#                    destinationIndex=3
#                    ,flux_constant=soilIn*Fs
#                ),
#                ConstantInFlux_by_PoolIndex(
#                    destinationIndex=4
#                    ,flux_constant=soilIn*Fm
#                )
#            )
#        )
#      } 
#    }
#    if(class(surfaceIn)=="data.frame" && class(soilIn)=='data.frame'){
#        in_times_surface=surfaceIn[,1]
#        in_times_soil=soilIn[,1]
#        in_vals_surface =surfaceIn[,2]
#        in_vals_soil =soilIn[,2]
#        inputFluxes=StateIndependentInFluxList_by_PoolIndex(
#            list(
#                StateIndependentInFlux_by_PoolIndex(
#                    destinationIndex=PoolIndex(1)
#                    ,flux=ScalarTimeMap(times=in_times_surface,data=Fs*in_vals_surface)
#                ),
#                StateIndependentInFlux_by_PoolIndex(
#                    destinationIndex=PoolIndex(2)
#                    ,flux=ScalarTimeMap(times=in_times_surface,data=Fm*in_vals_surface)
#                ),
#                StateIndependentInFlux_by_PoolIndex(
#                    destinationIndex=PoolIndex(3)
#                    ,flux=ScalarTimeMap(times=in_times_soil,data=Fs*in_vals_soil)
#                ),
#                StateIndependentInFlux_by_PoolIndex(
#                    destinationIndex=PoolIndex(4)
#                    ,flux=ScalarTimeMap(times=in_times_soil,data=Fm*in_vals_soil)
#                )
#            )
#       )
#    }
    Txtr=clay+silt
    fTxtr=1-0.75*Txtr
    Es=0.85-0.68*Txtr
    alpha51=(1-Ls)*(1-0.45); alpha61=Ls*(1-0.3); alpha52=1-0.55
    alpha53=(1-Ls)*(1-0.55); alpha54=1-0.55; alpha63=Ls*(1-0.3)
    alpha65=1-Es-0.004; alpha75=0.004; alpha76=0.03; alpha56=0.42; alpha57=1-0.55

    K=diag(ks)
    K[1,1]=ks[1]*exp(-3*Ls)
    K[3,3]=ks[3]*exp(-3*Ls)
    K[5,5]=ks[5]*fTxtr

    T=diag(-1,7,7)
    T[5,1]=alpha51
    T[6,1]=alpha61
    T[5,2]=alpha52
    T[5,3]=alpha53
    T[5,4]=alpha54
    T[6,3]=alpha63
    T[6,5]=alpha65
    T[7,5]=alpha75
    T[7,6]=alpha76
    T[5,6]=alpha56
    T[5,7]=alpha57

    A=T%*%K
    
    # whatever format xi is given in we convert it to a time map object
    # (function,constant,data.frame,list considering also the xi_lag argument)
    if(class(xi) == 'numeric' && length(xi)==1){
      xi=ScalarTimeMap(data=xi,lag=xi_lag)
    }
    if(inherits(xi, 'data.frame')) {
     xi=ScalarTimeMap(map=xi, lag=xi_lag)
    }
    if(inherits(xi, 'function')) {
     xi=ScalarTimeMap(map=xi, lag=xi_lag)
    }
    #fX=getFunctionDefinition(xi)
    At=ConstLinDecompOpWithLinearScalarFactor(mat=A,xi=xi)
    Fc=BoundFc(map=inputFc,lag=lag,format="Delta14C")
    # At the moment we still create an old style Matrix vector based model 
    # but with the ingredients provided in this more specific form we can later
    # build Instances of more specific Model classes.
    Mod=GeneralModel_14(t=t,A=At,ivList=C0,initialValF=ConstFc(F0_Delta14C, "Delta14C"),inputFluxes=inputFluxes,inputFc=Fc, di=lambda,solverfunc=solver,pass=FALSE)
    return(Mod)
  }
