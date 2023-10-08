#' Implementation of the Century model
#' 
#' This function implements the Century model as described in Parton et al.
#' (1987).
#' 
#' 
#' @param t A vector containing the points in time where the solution is
#' sought.
#' @param ks A vector of length 7 containing the values of the decomposition
#' rates for the different pools. Units in per week.
#' @param C0 A vector of length 7 containing the initial amount of carbon for
#' the 7 pools.
#' @param surfaceIn A scalar or data.frame object specifying the amount of aboveground litter
#' inputs to the soil surface by time (mass per area per week).
#' @param soilIn A scalar or data.frame object specifying the amount of belowground litter
#' inputs to the soil by time (mass per area per week).
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
#' @param solver A function that solves the system of ODEs. This can be
#' \code{\link{euler}} or \code{\link{deSolve.lsoda.wrapper}} or any other user
#' provided function with the same interface.
#' @return A Model Object that can be further queried
#' @details This is one of the few examples that internally make use of
#' the new infrastructure for flux based descriptions of models (see examples). 
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
#' mnths=seq(0,100)
#' APPT=50 # Assume 50 cm annual precipitation
#' Pmax=-40+7.7*APPT # Max aboveground production
#' Rmax=100+7.0*APPT # Max belowground production
#' abvgIn=Pmax/(Pmax+Rmax)
#' blgIn=Rmax/(Pmax+Rmax)
#' 
#' cm=CenturyModel(t=mnths, surfaceIn = abvgIn, soilIn = blgIn, LN=0.5, Ls=0.1)
#' Ct=getC(cm)
#' 
#' poolNames=c("Surface structural", "Surface metabolic", "Belowground structural",
#'                "Belowground metabolic", "Active SOM", "Slow SOM", "Passive SOM")
#' matplot(mnths,Ct, type="l", lty=1, col=1:7, xlab="Time (months)", ylab="Carbon stock ")
#' legend("topleft", poolNames, lty=1, col=1:7, bty="n")

CenturyModel<- function 
  (t,      
   ks=c(STR.surface=0.076, MET.surface=0.28, STR.belowgroun=0.094,
     MET.belowground=0.35,ACT=0.14,SLW=0.0038,PAS=0.00013),
   C0=rep(0, 7),	
   surfaceIn,    
   soilIn,    
   LN, 
   Ls, 
   clay=0.2, 
   silt=0.45, 
   xi=1,  
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
   
    if(inherits(surfaceIn, 'numeric') && inherits(soilIn, 'numeric')) {
      if(length(surfaceIn)==1 && length(soilIn)==1){
        inputFluxes=ConstantInFluxList_by_PoolIndex(
            list(
                ConstantInFlux_by_PoolIndex(
                    destinationIndex=1
                    ,flux_constant=surfaceIn*Fs
                ),
                ConstantInFlux_by_PoolIndex(
                    destinationIndex=2
                    ,flux_constant=surfaceIn*Fm
                ),
                ConstantInFlux_by_PoolIndex(
                    destinationIndex=3
                    ,flux_constant=soilIn*Fs
                ),
                ConstantInFlux_by_PoolIndex(
                    destinationIndex=4
                    ,flux_constant=soilIn*Fm
                )
            )
        )
      } 
    }
    if(inherits(surfaceIn, "data.frame") && inherits(soilIn, 'data.frame')){
        in_times_surface=surfaceIn[,1]
        in_times_soil=soilIn[,1]
        in_vals_surface =surfaceIn[,2]
        in_vals_soil =soilIn[,2]
        inputFluxes=StateIndependentInFluxList_by_PoolIndex(
            list(
                StateIndependentInFlux_by_PoolIndex(
                    destinationIndex=PoolIndex(1)
                    ,flux=ScalarTimeMap(times=in_times_surface,data=Fs*in_vals_surface)
                ),
                StateIndependentInFlux_by_PoolIndex(
                    destinationIndex=PoolIndex(2)
                    ,flux=ScalarTimeMap(times=in_times_surface,data=Fm*in_vals_surface)
                ),
                StateIndependentInFlux_by_PoolIndex(
                    destinationIndex=PoolIndex(3)
                    ,flux=ScalarTimeMap(times=in_times_soil,data=Fs*in_vals_soil)
                ),
                StateIndependentInFlux_by_PoolIndex(
                    destinationIndex=PoolIndex(4)
                    ,flux=ScalarTimeMap(times=in_times_soil,data=Fm*in_vals_soil)
                )
            )
       )
    }
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
    
#    # whatever format xi is given in we convert it to a time map object
#    # (function,constant,data.frame,list considering also the xi_lag argument)
#    if(inherits(xi, 'numeric') && length(xi)==1){
#      xi=ScalarTimeMap(data=xi,lag=xi_lag)
#    }
#    if(inherits(xi, 'data.frame')) {
#     xi=ScalarTimeMap(map=xi, lag=xi_lag)
#    }
#    if(inherits(xi, 'function')) {
#     xi=ScalarTimeMap(map=xi, lag=xi_lag)
#    }
#    #fX=getFunctionDefinition(xi)
#    At=ConstLinDecompOpWithLinearScalarFactor(mat=A,xi=xi)

# I had to disable the previous functionality above and return to the old functionality.
# Tests have to be implemented to make sure the new functionality works and can compile documentation.
      if(length(xi)==1) fX=function(t){xi}
      if(inherits(xi, "data.frame")){
        X=xi[,1]
        Y=xi[,2]
        fX=function(t){as.numeric(spline(X,Y,xout=t)[2])}
       }
      At=BoundLinDecompOp(
        function(t){fX(t)*A},
        t_start,
        t_end
      )

    
    # At the moment we still create an old style Matrix vector based model 
    # but with the ingredients provided in this more specific form we can later
    # build Instances of more specific Model classes.
    Mod=GeneralModel(t=t,A=At,ivList=C0,inputFluxes=inputFluxes,solverfunc=solver,pass=FALSE)
    return(Mod)
  }
