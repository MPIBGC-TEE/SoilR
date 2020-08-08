#' Implementation of the Century model
#' 
#' This function implements the Century model as described in Parton et al.
#' (1987).
#' 
#' 
#' @param t A vector containing the points in time where the solution is
#' sought.
#' @param ks A vector of length 5 containing the values of the decomposition
#' rates for the different pools. Units in per week.
#' @param C0 A vector of length 5 containing the initial amount of carbon for
#' the 5 pools.
#' @param In A scalar or data.frame object specifying the amount of litter
#' inputs by time (mass per area per week).
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
#' @example ./inst/tests/src/runit.Century.R
CenturyModel<- function 
  (t,      
   ks=c(k.STR=0.094,k.MET=0.35,k.ACT=0.14,k.SLW=0.0038,k.PAS=0.00013),  
   C0=c(0,0,0,0,0),	
   In,    
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
    if(length(ks)!=5) stop("ks must be of length = 5")
    if(length(C0)!=5) stop("the vector with initial conditions must be of length = 5")
    Fm=0.85-0.18*LN
    Fs=1-Fm
    #if(length(In)==1){
    #  inputFluxes=BoundInFluxes(
    #    function(t){matrix(nrow=5,ncol=1,c(In*Fm,In*Fs,0,0,0))},
    #    t_start,
    #    t_end
    #  )
    #}
   
    if(class(In)=='numeric') {
      if(length(In)==1){
        inputFluxes=ConstantInFluxList_by_PoolIndex(
            list(
                ConstantInFlux_by_PoolIndex(
                    destinationIndex=1
                    ,flux_constant=In*Fm
                )
                ,
                ConstantInFlux_by_PoolIndex(
                    destinationIndex=2
                    ,flux_constant=In*Fs
                )
            )
        )
      } 
    }
    if(class(In)=="data.frame"){
        # we want to be as specific as possible
        # and especially retain the information that only
        # 2 of the 5 pools receive inputs.

        # This would be lost in the old style description as a vector valued
        # function, since R cannot detect which pools are permanently zero in 
        # a vector:
        # if(class(In)=="data.frame"){
        #   x=In[,1]  
        #   y=In[,2]  
        #   inputFlux=splinefun(x,y)
        #   inputFluxes=BoundInFluxes(
        #     function(t){
        #       matrix(
        #        nrow=5,
        #        ncol=1,
        #        c(inputFlux(t)*Fm,inputFlux(t)*Fs,0,0,0)
        #       ) 
        #     },
        #     min(x),
        #     max(x)
        #   )
        # }

        # We therefore describe only the non zero fluxes
        in_times=In[,1]
        in_vals =In[,2]
        inputFluxes=StateIndependentInFluxList_by_PoolIndex(
            list(
                StateIndependentInFlux_by_PoolIndex(
                    destinationIndex=PoolIndex(1)
                    ,flux=ScalarTimeMap(times=in_times,data=Fm*in_vals)
                )
                ,
                StateIndependentInFlux_by_PoolIndex(
                    destinationIndex=PoolIndex(2)
                    ,flux=ScalarTimeMap(times=in_times,data=Fs*in_vals)
                )
            )
       )
    }
    Txtr=clay+silt
    fTxtr=1-0.75*Txtr
    Es=0.85-0.68*Txtr
    alpha31=0.45
    alpha32=0.55
    alpha34=0.42
    alpha35=0.45
    alpha41=0.3 
    alpha53=0.004
    alpha43=1-Es-alpha53
    alpha54=0.03
    A=-1*diag(abs(ks))
    A[1,1]=A[1,1]*exp(-3*Ls)
    A[3,3]=A[3,3]*fTxtr
    A[3,1]=alpha31*abs(A[1,1])
    A[3,2]=alpha32*abs(A[2,2])
    A[3,4]=alpha34*abs(A[4,4])
    A[3,5]=alpha35*abs(A[5,5])
    A[4,1]=alpha41*abs(A[1,1])
    A[4,3]=alpha43*abs(A[3,3])
    A[5,3]=alpha53*abs(A[3,3])
    A[5,4]=alpha54*abs(A[4,4])
    # whatever format xi is given in we convert it to a time map object
    # (function,constant,data.frame,list considering also the xi_lag argument)
    if(class(xi) == 'numeric' && length(xi)==1){
      xi=ScalarTimeMap(data=xi,lag=xi_lag)
    }
    if(class(xi)=='data.frame') {
     xi=ScalarTimeMap(map=xi, lag=xi_lag)
    }
    if(class(xi)=='function') {
     xi=ScalarTimeMap(map=xi, lag=xi_lag)
    }
    #fX=getFunctionDefinition(xi)
    At=ConstLinDecompOpWithLinearScalarFactor(mat=A,xi=xi)
    
    # At the moment we still create an old style Matrix vector based model 
    # but with the ingredients provided in this more specific form we can later
    # build Instances of more specific Model classes.
    Mod=GeneralModel(t=t,A=At,ivList=C0,inputFluxes=inputFluxes,solverfunc=solver,pass=FALSE)
    return(Mod)
  }
